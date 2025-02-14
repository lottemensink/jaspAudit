#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
# When making changes to this file always mention @koenderks as a
# reviewer in the pull Request.

auditBayesianOptionalStopping <- function(jaspResults, dataset, options, ...) {
  dataset <- .jfaReadData(options, jaspResults, stage = "evaluation")
  evaluationContainer <- .jfaAddStageContainer(jaspResults, stage = "evaluation", position = 1)
  .jfaAddIndicator(options, jaspResults, dataset)
  evaluationOptions <- .jfaInputOptionsGather(options, dataset, jaspResults, stage = "planning")
  evaluationStateOptStop <- .jfaEvaluationOptStop(options, dataset, evaluationContainer)
  .jfaTableNumberInit(jaspResults)
  .jfaFigureNumberInit(jaspResults)
  .addExplanation(options, jaspResults, evaluationStateOptStop, evaluationContainer, positionInContainer = 1)
  .createTable(options, jaspResults, dataset, evaluationStateOptStop, evaluationContainer, positionInContainer = 2)
  .addConclusion(options, jaspResults, evaluationStateOptStop, evaluationContainer, positionInContainer = 4)
  .jfaPlotOptStop(options, jaspResults, evaluationStateOptStop, dataset, evaluationContainer, positionInContainer = 5)
  .jfaPlotEstimates(options, evaluationStateOptStop, evaluationContainer, jaspResults, positionInContainer = 7)
  .jfaPlotPriorAndPosterior(options, evaluationOptions, evaluationStateOptStop, evaluationContainer, jaspResults, positionInContainer = 9, stage = "evaluation")
  .jfaTablePriorPosterior(options, evaluationOptions, evaluationStateOptStop, evaluationContainer, jaspResults, positionInContainer = 11, stage = "evaluation")
  .jfaPlotSObjectives(options, evaluationOptions, evaluationStateOptStop, evaluationContainer, jaspResults, positionInContainer = 13)
  .jfaTableTaints(options, dataset, evaluationContainer, jaspResults, positionInContainer = 15)
  .jfaPlotSObjectives(options, evaluationOptions, evaluationStateOptStop, evaluationContainer, jaspResults, positionInContainer = 17)
}

.jfaAddIndicator <- function(options, jaspResults, dataset) {
  if (is.null(jaspResults[["indicator_col"]])) {
    jaspResults[["indicator_col"]] <- createJaspColumn(columnName = options[["indicator_col"]], dependencies = "indicator_col")
    dataset[[options[["indicator_col"]]]] <- rep(0, nrow(dataset))
    initialSelection <- sample(1:nrow(dataset), 1)
    dataset[[options[["indicator_col"]]]][initialSelection] <- 1
  }
}

.createTable <- function(options, jaspResults, dataset, evaluationStateOptStop, evaluationContainer, positionInContainer) {
  if (!is.null(jaspResults[["bosTable"]])) {
    return()
  }
  .jfaTableNumberUpdate(jaspResults)
  title <- gettextf("<b>Table %1$i.</b> Evaluation Summary", jaspResults[["tabNumber"]]$object)
  tb <- createJaspTable(title)
  tb$transpose <- TRUE
  tb$position <- positionInContainer
   tb$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  tb$addColumnInfo(name = "null", title = "", type = "string")
  tb$addColumnInfo(name = "alpha_risk", title = gettext("Allowable risk of incorrect acceptance"), type = "number")
  tb$addColumnInfo(name = "beta_risk", title = gettext("Allowable risk of incorrect rejection"), type = "number")
  if (options[["materiality_test"]]) {
    tb$addColumnInfo(name = "materiality", title = gettext("Performance materiality"), type = "number")
  }
  if (options[["min_precision_test"]]) {
    tb$addColumnInfo(name = "min_precision", title = gettext("Min. precision"), type = "number")
  }
  tb$addColumnInfo(name = "n", title = gettext("Sample size"), type = "integer")
  tb$addColumnInfo(name = "x", title = gettext("Misstatements"), type = "integer")
  tb$addColumnInfo(name = "t", title = gettext("Taint"), type = "number")
  tb$addColumnInfo(name = "bf", title = gettextf("BF%1$s", "\u208B\u208A"), type = "number")
  evaluationContainer[["bosTable"]] <- tb
  if (is.null(evaluationContainer[["evaluationState"]])) {
    tb$addFootnote(
      message = gettext("Either the materiality is defined as zero, or one of the required variables is missing."),
      symbol = gettextf("%1$s <b>Insufficient information.</b>", "\u26A0")
    )
    return()
  }
  tb[["alpha_risk"]] <- options[["alpha_risk"]]
  tb[["beta_risk"]] <- options[["beta_risk"]]
  tb[["materiality"]] <- evaluationStateOptStop[["materiality"]]
  tb[["n"]] <- evaluationStateOptStop[["n"]]
  tb[["x"]] <- evaluationStateOptStop[["x"]]
  tb[["t"]] <- evaluationStateOptStop[["t"]]
  tb[["bf"]] <- evaluationStateOptStop[["posterior"]][["hypotheses"]]$bf.h1
}


.jfaEvaluationOptStop <- function(options, dataset, evaluationContainer) {
  if (!is.null(evaluationContainer[["evaluationState"]])) {
    return(evaluationContainer[["evaluationState"]]$object)
  }
  # check whether there is enough data to perform analysis
  if (!options[["materiality_test"]] && !options[["min_precision_test"]]) {
    return()
  } else if (options[["materiality_test"]] && options[["materiality_rel_val"]] == 0) {
    return()
  } else if (options[["dataType"]] %in% c("data", "pdata") && (options[["values.audit"]] == "" || options[["id"]] == "")) {
    return()
  }
  if (options[["dataType"]] %in% c("data", "pdata") && options[["values.audit"]] != "" && !all(unique(dataset[[options[["values.audit"]]]]) %in% c(0, 1)) && options[["values"]] == "") {
    return()
  }
  materiality <- if (options[["materiality_type"]] == "materiality_rel") options[["materiality_rel_val"]] else options[["materiality_abs_val"]]
  prior <- jfa::auditPrior(materiality = materiality, expected = options[["expected_pop_rate"]], likelihood = "binomial",
    method = "impartial"
  )
  conf_level <- 1 - options[["alpha_risk"]]

  # select evaluation method
  if (all(unique(dataset[[options[["values.audit"]]]]) %in% c(0, 1))) {
    bookValues <- rep(1, nrow(dataset))
    auditValues <- ifelse(dataset[[options[["values.audit"]]]] == 0, 1, 0)
    selected <- dataset[[options[["times"]]]]
    binaryData <- as.data.frame(cbind(bookValues, auditValues, selected))
    result <- try({
      jfa::evaluation(
        conf.level = conf_level, materiality = materiality,
        data = binaryData, values = "bookValues", values.audit = "auditValues",
        method = options[["method"]], N.units = options[["N.units"]],
        prior = prior, alternative = options[["area"]], times = "selected"
      )
    })
  } else {
    result <- try({
      jfa::evaluation(
        data = dataset, times = options[["indicator_col"]], conf.level = conf_level,
        materiality = materiality, alternative = options[["area"]],
        values = options[["values"]], values.audit = options[["values.audit"]],
        method = options[["method"]], N.items = options[["N.items"]], N.units = options[["N.units"]],
        prior = prior
      )
    })
  }
  evaluationContainer[["evaluationState"]] <- createJaspState(result)
  evaluationContainer[["evaluationState"]]$dependOn(c(
    "materiality_rel_val", "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  return(result)
}

.addExplanation <- function(options, jaspResults, evaluationStateOptStop, evaluationContainer, positionInContainer) {
  if (!is.null(evaluationContainer[["explanation"]])) {
    return()
  }
  if (is.null(evaluationContainer[["evaluationState"]])) {
    return()
  }
  materiality <- if (options[["materiality_type"]] == "materiality_rel") options[["materiality_rel_val"]] else options[["materiality_abs_val"]]

  introduction <- createJaspHtml(gettextf("The purpose of Bayesian optional stopping is to sequentially evaluate the audit sample. Instead of determining the sample size in advance, the evidence is monitored while the sample is being collected. As soon as the sample provides the desired assurance, data collection can be stopped. The allowable risk of incorrect acceptance is %1$s and the allowable risk of incorrect rejection is %2$s Furthermore, the performance materiality is set at %3$s", 
  options[["alpha_risk"]], options[["beta_risk"]], materiality)) 

  introduction$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  introduction$position <- positionInContainer
  evaluationContainer[["introduction"]] <- introduction


}
.addConclusion <- function(options, jaspResults, evaluationStateOptStop, evaluationContainer, positionInContainer) {
  if (!is.null(evaluationContainer[["conclusion"]])) {
    return()
  }
  if (is.null(evaluationContainer[["evaluationState"]])) {
     return()
  }
  threshold_acc <- round((1 - options[["alpha_risk"]]) / options[["alpha_risk"]], 3)
  threshold_rej <- round(options[["beta_risk"]] / (1 - options[["beta_risk"]]), 3)
  bf <- round(evaluationStateOptStop[["posterior"]][["hypotheses"]]$bf.h1, 3)
 explanation <- gettextf(
    "The Bayes factor threshold for accepting the population of financial statements is %1$s. The Bayes factor threshold for rejecting the population of financial statements is %2$s. The statististical sample evaluation, based on an impartial prior with an expected misstatement rate of %3$s, yields a Bayes factor of %4$s. \n\n",
    threshold_acc, threshold_rej, options[["expected_pop_rate"]], bf
  )
  if (bf > threshold_acc) {
    conclusion <- createJaspHtml(gettextf(
      "%1$sAs the Bayes factor is greater than the Bayes factor threshold for accepting the population, data collection can be terminated. The population can be accepted, with a %2$s percent risk that this conclusion is incorrect.",
      explanation, options[["alpha_risk"]] * 100
    ))
  } else if (bf < threshold_rej) {
    conclusion <- createJaspHtml(gettextf(
      "%1$sAs the Bayes factor is smaller than the Bayes factor threshold for rejecting the population, the auditor can terminate data collection. The population can be rejected, with a %2$s percent risk that this conclusion is incorrect.",
      explanation, options[["beta_risk"]] * 100
    ))
  } else {
    conclusion <- createJaspHtml(gettextf("%1$sAs neither of the Bayes factor thresholds are surpassed, a conclusion cannot be drawn. Data collection should continue.", 
      explanation
      ))
  }
  conclusion$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  conclusion$position <- positionInContainer
  evaluationContainer[["conclusion"]] <- conclusion
}

.jfaPlotOptStop <- function(options, jaspResults, evaluationStateOptStop, dataset, evaluationContainer, positionInContainer) {
  if (!is.null(evaluationContainer[["seqPlot"]])) {
    return()
  }
  if (is.null(evaluationContainer[["evaluationState"]])) {
    return()
  }
  .jfaFigureNumberUpdate(jaspResults)

  fg <- createJaspPlot(title = gettext("Optional stopping with the Bayes factor"), width = 530, height = 400)
  fg$position <- positionInContainer
  fg$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  evaluationContainer[["seqPlot"]] <- fg
  caption <- createJaspHtml(gettextf(
    "<b>Figure %1$i.</b> Monitoring the Bayes factor throughout data collection.",
    jaspResults[["figNumber"]]$object
  ), "p")
  caption$position <- positionInContainer + 1
  evaluationContainer[["plotOptStopText"]] <- caption
  threshold_acc <- (1 - options[["alpha_risk"]]) / options[["alpha_risk"]]
  threshold_rej <- options[["beta_risk"]] / (1 - options[["beta_risk"]])
  bf <- c()
  materiality <- if (options[["materiality_type"]] == "materiality_rel") options[["materiality_rel_val"]] else options[["materiality_abs_val"]]
  prior <- jfa::auditPrior(
    materiality = materiality, expected = options[["expected_pop_rate"]], likelihood = "binomial",
    method = "impartial"
  )
  if (all(unique(dataset[[options[["values.audit"]]]]) %in% c(0, 1))) {
    bookValues <- rep(1, nrow(dataset))
    auditValues <- ifelse(dataset[[options[["values.audit"]]]] == 0, 1, 0)
    selected <- dataset[[options[["times"]]]]
    binaryData <- as.data.frame(cbind(bookValues, auditValues, selected))
    for (i in seq_len(nrow(dataset))){
      bf[i] <- jfa::evaluation(
        conf.level = options[["conf_level"]], materiality = materiality,
        data = binaryData[1:i, ], values = "bookValues", values.audit = "auditValues",
        method = options[["method"]], N.units = options[["N.units"]],
        prior = prior, alternative = options[["area"]], times = "selected"
      )[["posterior"]][["hypotheses"]]$bf.h1
    }
  } else {
    for (i in seq_len(nrow(dataset))){
    bf[i] <- jfa::evaluation(
      data = dataset, times = options[["indicator_col"]], conf.level = options[["conf_level"]],
      materiality = materiality, alternative = options[["area"]],
      values = options[["values"]], values.audit = options[["values.audit"]],
      method = options[["method"]], N.items = options[["N.items"]], N.units = options[["N.units"]],
      prior = prior
      )[["posterior"]][["hypotheses"]]$bf.h1
    }
  }
yRange <- log(range(bf))
if (all(abs(yRange) <= log(100))) {
  threshold_acc <- log(threshold_acc)
  threshold_rej <- log(threshold_rej)
} else {
  threshold_acc <- log(threshold_acc) * log10(exp(1))
  threshold_rej <- log(threshold_rej) * log10(exp(1))
}
p <- plot(evaluationStateOptStop, type = "sequential") +
  ggplot2::geom_hline(ggplot2::aes(yintercept = threshold_acc, linetype = "Threshold for acceptance"), color = "green") +
  ggplot2::geom_hline(ggplot2::aes(yintercept = threshold_rej, linetype = "Threshold for rejection"), color = "red") +
  ggplot2::scale_linetype_manual(name = "", values = c("Threshold for acceptance" = "dashed", "Threshold for rejection" = "dashed")) +
  ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(color = c("green", "red"), linetype = c("dashed", "dashed")), nrow = 1)) +
  ggplot2::theme(legend.position = "bottom", plot.margin = ggplot2::unit(c(1, 1, 2, 1), "lines"), axis.ticks.y.right = ggplot2::element_blank(), 
    axis.text.y.right = ggplot2::element_blank(), axis.title.y.right = ggplot2::element_blank()) +
  ggplot2::geom_segment(x = Inf, xend = Inf, y = -Inf, yend = Inf, color = "white") 
fg$plotObject <- p
}


.jfaPlotSObjectives <- function(options, prevOptions, parentState, parentContainer, jaspResults, positionInContainer = 5) {
  if (!options[["plotObjectives"]]) {
    return()
  }
  .jfaFigureNumberUpdate(jaspResults)
    figure <- createJaspPlot(
      plot = NULL, title = gettext("Evaluation of Sampling Objectives"),
      width = 600, height = 300
    )
    figure$position <- positionInContainer
    figure$dependOn(options = c("plotObjectives", "display"))
    parentContainer[["plotObjectives"]] <- figure
    materiality <- parentState[["materiality"]]
    min_precision <- options[["min_precision_rel_val"]]
    bound <- parentState[["ub"]]
    mle <- parentState[["mle"]]
    precision <- parentState[["precision"]]
      
      objectiveColor <- "orange"
      boundColor <- if (bound < materiality) rgb(0, 1, .7, 1) else rgb(1, 0, 0, 1)
      precisionColor <- if (precision < min_precision) rgb(0, 1, .7, 1) else rgb(1, 0, 0, 1)

        label <- rev(c(gettext("Performance materiality"), gettext("Upper bound"), gettext("Most likely error")))
        values <- rev(c(materiality, bound, mle))
        fill <- rev(c(objectiveColor, boundColor, "#1380A1"))
      plotData <- data.frame(x = label, y = values)
      plotData$x <- factor(plotData$x, levels = plotData$x)
      if (all(plotData$y > 0)) {
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1.2 * max(values)), min.n = 4)
        yLimits <- c(0, max(yBreaks))
      } else {
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(min(values), 1.2 * max(values)), min.n = 4)
        yLimits <- range(yBreaks)
      }
      if (options[["display"]] == "amount") {
        yLabels <- format(yBreaks, scientific = FALSE)
        valueLabels <- ceiling(values)
      } else {
        yLabels <- paste0(round(yBreaks * 100, 2), "%")
        valueLabels <- paste0(round(values * 100, 2), "%")
      }
    plot <- ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_bar(stat = "identity", col = "black", size = 1, fill = fill) +
      ggplot2::coord_flip() +
      ggplot2::xlab(NULL) +
      ggplot2::annotate(
        geom = "text",
        y = values, x = 1:length(values), label = valueLabels,
        size = 6, vjust = 0.5, hjust = -0.1
      ) +
      ggplot2::scale_y_continuous(name = "", breaks = yBreaks, limits = yLimits, labels = yLabels) +
      jaspGraphs::geom_rangeframe(sides = "") +
      jaspGraphs::themeJaspRaw(legend.position = "none") +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(hjust = 0),
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb", size = 0.5)
      )
      
      figure$plotObject <- plot

  if (options[["explanatoryText"]]) {
    figureCaption <- createJaspHtml(gettextf(
      "<b>Figure %1$i.</b> Evaluation information for the current annotated selection. The materiality is compared with the maximum misstatement and the most likely misstatement. The most likely misstatement is the best estimate of the true misstatement in the population. The upper bound is an estimate of the maximum misstatement in the population.",
      jaspResults[["figNumber"]]$object
    ), "p")
    figureCaption$position <- positionInContainer + 1
    figureCaption$dependOn(optionsFromObject = parentContainer[["plotObjectives"]])
    figureCaption$dependOn(options = "explanatoryText")
    parentContainer[["evaluationInformationText"]] <- figureCaption
  }
}


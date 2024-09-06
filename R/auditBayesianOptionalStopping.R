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
  parentContainer <- .jfaAddStageContainer(jaspResults, stage = "evaluation", position = 1)
  evaluationStateOptStop <- .jfaEvaluationOptStop(options, dataset, parentContainer)
  .createTable(options, jaspResults, dataset, parentContainer)
  .addExplanation(options, jaspResults, evaluationStateOptStop)
  .addConclusion(options, jaspResults, evaluationStateOptStop)
}

.createTable <- function(options, jaspResults, dataset, parentContainer) {
  if (!is.null(jaspResults[["bosTable"]])) {
    return()
  }

  tb <- createJaspTable(gettext("Evaluation Output"))
  tb$transpose <- TRUE
  tb$position <- 1

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

  jaspResults[["bosTable"]] <- tb

  if (is.null(parentContainer[["evaluationState"]])) {
    return()
  } else {
    state <- parentContainer[["evaluationState"]]$object
  }

  tb[["alpha_risk"]] <- options[["alpha_risk"]]
  tb[["beta_risk"]] <- options[["beta_risk"]]
  tb[["materiality"]] <- state[["materiality"]]
  tb[["n"]] <- state[["n"]]
  tb[["x"]] <- state[["x"]]
  tb[["t"]] <- state[["t"]]
  tb[["bf"]] <- state[["posterior"]][["hypotheses"]]$bf.h1
}

.jfaEvaluationOptStop <- function(options, dataset, parentContainer) {
  if (!is.null(parentContainer[["evaluationState"]])) {
    return(parentContainer[["evaluationState"]]$object)
  }

  materiality <- if (options[["materiality_type"]] == "materiality_rel") options[["materiality_rel_val"]] else options[["materiality_abs_val"]]

  prior <- jfa::auditPrior(
    materiality = materiality, expected = options[["expected_pop_rate"]], likelihood = "binomial",
    method = "impartial"
  )

  conf_level <- 1 - options[["alpha_risk"]]

  result <- jfa::evaluation(
    conf.level = conf_level, materiality = materiality,
    n = nrow(dataset), x = length(which(dataset[[options[["values.audit"]]]] == 1)),
    method = "binomial", prior = prior
  )


  parentContainer[["evaluationState"]] <- createJaspState(result)

  parentContainer[["evaluationState"]]$dependOn(c(
    "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))

  return(result)
}


.addExplanation <- function(options, jaspResults, evaluationStateOptStop) {
  if (!is.null(jaspResults[["explanation"]])) {
    return()
  }

  threshold_acc <- round((1 - options[["alpha_risk"]]) / options[["alpha_risk"]], 3)
  threshold_rej <- round(options[["beta_risk"]] / (1 - options[["beta_risk"]]), 3)
  bf <- round(evaluationStateOptStop[["posterior"]][["hypotheses"]]$bf.h1, 3)

  htmlText <- createJaspHtml(gettextf(
    "The Bayes factor threshold for accepting the population of financial statements is %1$s. The Bayes factor threshold for rejecting the population of financial statements is %2$s. The statististical sample evaluation, based on an impartial prior with an expected misstatement rate of %3$s, yields a Bayes factor of %4$s.",
    threshold_acc, threshold_rej, options[["expected_pop_rate"]], bf
  ))

  htmlText$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  htmlText$position <- 2

  jaspResults[["explanation"]] <- htmlText
}

.addConclusion <- function(options, jaspResults, evaluationStateOptStop) {
  if (!is.null(jaspResults[["conclusion"]])) {
    return()
  }

  threshold_acc <- round((1 - options[["alpha_risk"]]) / options[["alpha_risk"]], 3)
  threshold_rej <- round(options[["beta_risk"]] / (1 - options[["beta_risk"]]), 3)
  bf <- evaluationStateOptStop[["posterior"]][["hypotheses"]]$bf.h1

  if (bf > threshold_acc) {
    conclusion <- createJaspHtml(gettextf(
      "As the Bayes factor is greater than the Bayes factor threshold for accepting the population, data collection can be terminated. The population can be accepted, with a %1$s percent risk that this conclusion is incorrect.",
      options[["alpha_risk"]] * 100
    ))
  } else if (bf < threshold_rej) {
    conclusion <- createJaspHtml(gettextf(
      "As the Bayes factor is smaller than the Bayes factor threshold for rejecting the population, the auditor can terminate data collection. The population can be rejected, with a %1$s percent risk that this conclusion is incorrect.",
      options[["beta_risk"]] * 100
    ))
  } else {
    conclusion <- createJaspHtml(gettextf("As neither of the Bayes factor thresholds are surpassed, a conclusion cannot be drawn. Data collection should continue."))
  }

  conclusion$dependOn(options = c(
    "alpha_risk", "beta_risk", "materiality_rel_val",
    "materiality_abs_val", "expected_pop_rate", "values.audit"
  ))
  conclusion$position <- 3

  jaspResults[["conclusion"]] <- conclusion
}
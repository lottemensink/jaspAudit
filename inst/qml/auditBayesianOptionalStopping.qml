// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.\
//
// When making changes to this file always mention @koenderks as a
// reviewer in the Pull Request
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import JASP.Widgets
import "./common" as Common
import "./common/planning" as Planning
import "./common/selection" as Selection
import "./common/evaluation" as Evaluation

Form
{
    columns: 1
    CheckBox { name: "bayesian"; checked: true; visible: false }
    RadioButtonGroup { name: "dataType"; visible: false; RadioButton { name: "data"; checked: true } }
    CheckBox { name: "workflow"; checked: false; visible: false }
    CheckBox { name: "separateMisstatement"; checked: false; visible: false }
    RadioButtonGroup { name: "expected_type"; visible: false; RadioButton { name: "expected_rel"; checked: true } }
    
    Evaluation.EvaluationVariablesList 
    {
        id: variables
        use_sample: true
    }

    Planning.SamplingObjectives 
    {
        id: objectives
        show_confidence: true 
    }
    
    Common.ExplanatoryText { }

        
    CheckBox
    {
    id:                     thresholds_check
    text:                   qsTr("Optional stopping with thresholds")
    name:                   "thresholds_check"
    checked:                true
    info:                   qsTr("Bayesian optional stopping allows the auditor to specify Bayes factor thresholds that determine when data collection can be terminated based on the allowable risks to draw an incorrect conclusion.")
    DoubleField
    {
        name:               "alpha_risk"
        label:              qsTr("Allowable risk of incorrect acceptance")
        min:                0
        defaultValue:       0.05
        visible:            threshold_check == true
        info:               qsTr("The allowable risk of incorrect acceptance refers to the allowable risk that the auditor concludes that the financial statements are free of material misstatement when they are actually materially misstated. In other words, it refers to the risk to incorrectly accept the population of financial statements")
    }
        DoubleField
    {
        name:               "beta_risk"
        label:              qsTr("Allowable risk of incorrect rejection")
        min:                0
        defaultValue:       0.05
        visible:            threshold_check == true
        info:               qsTr("The allowable risk of incorrect rejection refers to the allowable risk that the auditor concludes that the financial statements are materially misstated when they are actually free of material misstatement. In other words, it refers to the risk to incorrectly reject the population of financial statements.")
    }
    }

    Group
    {
    title:                  qsTr("Impartial prior elicitation")
    enabled:                enable
    info:                   qsTr("Choose most likely misstatement to construct the impartial prior distribution.")
    Planning.ExpectedPopRate { }
    }

    Section
    {
        title: qsTr("Report")
        Evaluation.EvaluationOutput
        {
            bayesian: true
            enable_taints: true
            enable_corrections: false
            enable_assumptions: false
            enable_objectives: true
            enable_predictive: false
            enable_scatter: true
            enable_estimates: true
        }
        Common.Display { }
    }
    Section
    {
        title: qsTr("Advanced")
        columns: 3

    Evaluation.IntervalType { bayesian: true; test: objectives.use_materiality }

  }
       
}
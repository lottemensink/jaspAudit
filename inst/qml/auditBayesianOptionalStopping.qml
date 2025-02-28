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
    RadioButtonGroup { name: "dataType"; visible: false; RadioButton { name: "pdata"; checked: true } }
    CheckBox { name: "workflow"; checked: false; visible: false }
    CheckBox { name: "separateMisstatement"; checked: false; visible: false }
    RadioButtonGroup { name: "expected_type"; visible: false; RadioButton { name: "expected_rel"; checked: true } }


	VariablesForm
		{
			id: 									variablesFormPlanning
			preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight
			enabled:								!pasteVariables.checked

			AvailableVariablesList
			{
				name: 								"variablesFormPlanning"
			}

			Selection.IdVariable { id: id }
			Selection.BookVariable { id: values }
		}

    Section
        {
            title: qsTr("Objectives")

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
    }

    Section
    {
        title: qsTr("Execution")

        Evaluation.Annotation { id: annotation; enable: !pasteVariables.checked; enable_values: values.use_book }
        Evaluation.AddVariables { id: names; enable: !pasteVariables.checked }

        CheckBox
            {
                id:                                 pasteVariables
                visible:                            false
                name:                               "pasteVariables"
                checked:                            false
            }

        Button
        {
            id: 								pasteButton
            text: 								qsTr("<b>Continue</b>")
            enabled: 							names.indicator_name != "" && names.variable_name != "" && !pasteVariables.checked && id.use_id
            onClicked:
            {
                pasteVariables.checked 		= true
                performAuditTable.colName   = names.variable_name
                performAuditTable.filter    = names.indicator_name + " > 0"
            }
        }

        Section
		{
			id: 									executeAuditSection
			title:									qsTr("Sample Selection")
			expanded:								pasteVariables.checked
			enabled:								pasteVariables.checked
			columns:								1

			Label
			{
				id: 								increaseSample
				Layout.alignment: 					Qt.AlignHCenter
				text: 								qsTr("<b>Increase your sample size.</b>") 
				visible: 							pasteVariables.checked
			}


            Slider
		    {
                name: "sample_size"
                min: 1
                max: 99999
                value: 1
                decimals: 0
                vertical: true
                id: dataSlider
                visible: false
                onValueChanged: {
                    moved()
                    performAuditTable.initialValuesSource = annotation.use_values ? "values" : ""
                }
		    }

            Row 
            {
                spacing: 10 

                Button
                {
                    id:                                 increaseSample1
                    text:                               qsTr("<b>+ 1</b>")
                    onClicked:  {
                        dataSlider.value += 1
                        performAuditTable.filter = names.indicator_name + " > 0"
                        performAuditTable.initialValuesSource = annotation.use_values ? "values" : ""

                }
                }
                Button
                {
                    id:                                 increaseSample3
                    text:                               qsTr("<b>+ 3</b>")
                    onClicked:  {
                        dataSlider.value += 3
                        performAuditTable.filter = names.indicator_name + " > 0"

                }
                }

                Button
                {
                    id:                                 increaseSample5
                    text:                               qsTr("<b>+ 5</b>")
                    onClicked:   {
                        dataSlider.value += 5
                        performAuditTable.filter = names.indicator_name + " > 0"

                }
                }

                Button
                {
                    id:                                 increaseSample10
                    text:                               qsTr("<b>+ 10</b>")
                    onClicked: {
                        dataSlider.value += 10
                        performAuditTable.filter = names.indicator_name + " > 0"

                    }                         

                }
            }

			Label
			{
				id: 								performAuditText
				Layout.alignment: 					Qt.AlignHCenter
				text: 								annotation.use_values ? qsTr("<b>Annotate your selected items with their audit (true) values.</b>") : qsTr("<b>Annotate your selected items as correct (0) or incorrect (1).</b>")
				visible: 							pasteVariables.checked
			}

			TableView
			{
				id:									performAuditTable
				name:								"performAudit"
				Layout.fillWidth: 					true
				modelType:							JASP.FilteredDataEntryModel
				source:     						["id", "values"]
				defaultValue:						0
				decimals:							10
				minimum:							-Infinity
				Layout.preferredHeight:				250 * preferencesModel.uiScale
                initialValuesSource:                annotation.use_values ? "values" : ""
			}
		}


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
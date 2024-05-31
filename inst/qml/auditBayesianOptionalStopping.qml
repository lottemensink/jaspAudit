import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import JASP.Widgets

import "./common" as Common
import "./common/planning" as Planning

Form
{
	// Hidden option(s)
	CheckBox { name: "workflow"; checked: false; visible: false }
	CheckBox { name: "bayesian"; checked: true; visible: false }
	RadioButtonGroup { name: "dataType"; visible: false; RadioButton { name: "data"; checked: true } }

    columns: 1
    info: qsTR("Bayesian Optional Stopping allows the user to monitor the audit evidence as it comes in, and decide when to stop data collection based on the strength of the observed evidence. Please see the manual of the Audit module (download here) for more detailed information about this analysis.")

    Group 
    {
        title: qsTr("Sampling Objectives")

       Label
		{
			text: 						qsTr("Performance materiality")
        }
			RadioButtonGroup
			{
				id: 					materiality_type
				name: 					"materiality_type"
				visible: 				materiality_test.checked
				info:					qsTr("Specify how the performance materiality is defined.")

				RadioButton
				{
					id:					materiality_relative
					name: 				"materiality_rel"
					text: 				qsTr("Relative")
					checked:			true
					childrenOnSameRow: 	true
					info:				qsTr("Specify the performance materiality as a percentage relative to the total number of units in the population.")

					PercentField
					{
						id:				materiality_relative_value
						visible: 		materiality_relative.checked
						decimals: 		2
						defaultValue: 	1
						min:			0.01
						max:			99.99
						name: 			"materiality_rel_val"
						info:			qsTr("The percentage associated with the performance materiality.")
					}
				}

				RadioButton
				{
					id: 				materiality_absolute
					name: 				"materiality_abs"
					text: 				qsTr("Absolute")
					childrenOnSameRow: 	true
					info:				qsTr("Specify the performance materiality as an absolute value in monetary units.")

					DoubleField
					{
						id:				materiality_absolute_value
						visible: 		materiality_absolute.checked
						name: 			"materiality_abs_val"
						defaultValue: 	0
						min: 			0
						fieldWidth: 	90 * preferencesModel.uiScale
						decimals: 		2
						info:			qsTr("The value associated with the performance materiality.")
					}
				}
			}

        
    
    Group
    {
        readonly	property bool	expected_is_relative:	expected_rel.checked
                    property bool	enable:					true
                    property bool	show_all:				false
                    property bool   enable_all:				false

        id: 					expected
        name: 					"expected_type"
        title: 					qsTr("Expected Misstatements")
        enabled:				enable
        info:					qsTr("The expected misstatements are the tolerable misstatements that can be found in the sample while still achieving the conditions outlined in the sampling objectives. It is advised to set this value conservatively to minimize the probability of the observed misstatements exceeding the expected misstatements, which would imply that insufficient work has been done in the end.")


            PercentField
            {
                text:           qsTr("Relative")
                id:				expected_rel_val
                name: 			"expected_rel_val"
                decimals: 		2
                defaultValue: 	0
                info:			qsTr("The percentage of expected misstatements in the sample.")
            }

    }


    Group
    {
        title:                  qsTr("Allowable risks")

            PercentField
            {
                text:           qsTr("Alpha")
                id:             alpha
                name:           "alpha"
                decimals:       2
                defaultValue:   5.00
            }



            PercentField
            {
                text:               qsTr("Beta")
                id:             beta
                name:           "beta"
                decimals:       2
                defaultValue:   5.00
            }
    


    }



    }



}
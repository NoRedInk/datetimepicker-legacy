module DateTimePicker.Styles exposing (..)

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Css exposing (..)
import Nri.Ui.Colors.V1 as Colors


timePickerDialog : Style
timePickerDialog =
    Css.batch
        [ float left
        , textAlign center
        , borderLeft3 (px 1) solid Colors.gray85
        ]


highlightStyle : Css.Style
highlightStyle =
    Css.batch
        [ backgroundColor Colors.azure
        , color Colors.white
        , borderRadius (px 0)
        , hover
            [ backgroundColor Colors.azureDark |> Css.important
            , color Colors.white
            ]
        ]


dayStyle : Css.Style
dayStyle =
    Css.batch
        [ padding4 (px 7) (px 7) (px 7) (px 9)
        , border (px 0)
        , textAlign right
        ]


cellStyle : Css.Style
cellStyle =
    Css.batch
        [ padding4 (px 7) (px 7) (px 7) (px 9)
        , border (px 0)
        , cursor pointer
        , hover [ backgroundColor Colors.glacier ]
        ]


tableStyle : Css.Style
tableStyle =
    Css.batch
        [ property "border-spacing" "0"
        , property "border-width" "0"
        , property "table-layout" "fixed"
        , margin (px 0)
        ]


emptyCellStyle : Css.Style
emptyCellStyle =
    Css.batch [ hover [ backgroundColor unset ], cursor unset ]

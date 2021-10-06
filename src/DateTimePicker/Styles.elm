module DateTimePicker.Styles exposing (..)

{-| -}

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
        [ property "box-shadow" "inset 0 0 10px 3px #3276b1"
        , backgroundColor Colors.azure
        , color Colors.frost
        , borderRadius (px 0)
        ]


dayStyle : Css.Style
dayStyle =
    Css.batch
        [ cellStyle
        , textAlign right
        ]


cellStyle : Css.Style
cellStyle =
    Css.batch
        [ padding4 (px 7) (px 7) (px 7) (px 9)
        , border (px 0)
        , cursor pointer
        ]


arrowStyle : Css.Style
arrowStyle =
    Css.batch
        [ boxSizing borderBox
        , textAlign center
        , transform (scale 0.8)
        , position absolute
        , padding2 (px 0) (px 8)
        , cursor pointer
        ]


headerStyle : Css.Style
headerStyle =
    Css.batch
        [ padding2 (px 10) (px 7)
        , backgroundColor Colors.gray96
        ]


tableStyle : Css.Style
tableStyle =
    Css.batch
        [ property "border-spacing" "0"
        , property "border-width" "0"
        , property "table-layout" "fixed"
        , margin (px 0)
        ]


activeStyle : Css.Style
activeStyle =
    Css.batch
        [ backgroundColor Colors.gray85
        , borderRadius (px 0)
        ]


emptyCellStyle : Css.Style
emptyCellStyle =
    Css.batch [ hover [ backgroundColor unset ], cursor unset ]

module DateTimePicker.Styles exposing (..)

{-| DateTimePicker.Css

Using [rtfeldman/elm-css](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest)
Include this in your elm-css port module to be included in your project's css file.


# Css

@docs css

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, class, descendants, td, tr, withClass)
import DateTimePicker.SharedStyles exposing (CssClasses(..))


{-| DatePicker's Css Stylesheet
-}
css =
    [ class DatePicker
        [ position relative ]
    , class Dialog
        [ fontFamilies [ "Arial", "Helvetica", "sans-serif" ]
        , fontSize (px 14)
        , borderBoxStyle
        , position absolute
        , border3 (px 1) solid darkGray
        , boxShadow4 (px 0) (px 5) (px 10) (rgba 0 0 0 0.2)
        , children dialogCss
        , property "z-index" "1"
        , displayFlex
        ]
    ]


dialogCss : List Snippet
dialogCss =
    [ class TimePickerDialog
        [ float left

        -- , height calendarHeight
        , textAlign center
        , borderLeft3 (px 1) solid darkGray
        , withClass DigitalTime digitalTimePickerDialogStyle
        , withClass AnalogTime analogTimePickerDialogStyle
        ]
    ]


analogTimePickerDialogStyle : List Css.Style
analogTimePickerDialogStyle =
    let
        timeHeaderStyle =
            Css.batch
                [ padding2 (px 3) (px 10)
                , marginTop (px 3)
                , marginBottom (px 3)
                , display inlineBlock
                , cursor pointer
                ]

        amPmStyle =
            Css.batch
                [ fontSize (Css.em 1.2)
                , padding2 (Css.em 1) (Css.em 0)
                , cursor pointer
                , margin2 (px 0) auto
                , width (px 85)
                , hover [ backgroundColor highlightedDay ]
                ]
    in
    [ width (px 230)
    , descendants
        [ class Header
            [ headerStyle
            , fontSize (Css.em 1.2)
            , descendants
                [ class Hour [ timeHeaderStyle ]
                , class Minute [ timeHeaderStyle ]
                , class AMPM [ timeHeaderStyle ]
                , class Active
                    [ activeStyle ]
                ]
            ]
        , class Body [ backgroundColor (hex "#fff"), padding2 (px 12) (px 15), height (px 202) ]
        , class AMPMPicker [ padding2 (px 40) (px 0) ]
        , class AM
            [ amPmStyle
            , withClass SelectedAmPm [ highlightStyle, hover [ highlightStyle ] ]
            ]
        , class PM
            [ amPmStyle
            , withClass SelectedAmPm [ highlightStyle, hover [ highlightStyle ] ]
            ]
        ]
    ]


digitalTimePickerDialogStyle : List Css.Style
digitalTimePickerDialogStyle =
    [ children
        [ class Header
            [ headerStyle
            ]
        , class Body
            [ backgroundColor (hex "#fff")
            , descendants
                [ Css.Foreign.table
                    [ tableStyle
                    , width (px 120)
                    , descendants
                        [ tr [ verticalAlign top ]
                        , td
                            [ width (pct 33)
                            , cellStyle
                            , hover
                                [ backgroundColor highlightedDay
                                , highlightBorderStyle
                                ]
                            , withClass EmptyCell [ emptyCellStyle ]
                            ]
                        , class SelectedHour [ highlightStyle, hover [ highlightStyle ] ]
                        , class SelectedMinute [ highlightStyle, hover [ highlightStyle ] ]
                        , class SelectedAmPm [ highlightStyle, hover [ highlightStyle ] ]
                        ]
                    ]
                ]
            ]
        ]
    ]


highlightStyle : Css.Style
highlightStyle =
    Css.batch
        [ property "box-shadow" "inset 0 0 10px 3px #3276b1"
        , backgroundColor selectedDate
        , color (hex "#fff")
        , highlightBorderStyle
        ]


highlightSelectedDay : Css.Color
highlightSelectedDay =
    hex "#d5e5f3"


selectedDate : Css.Color
selectedDate =
    hex "#428bca"


fadeText : Css.Color
fadeText =
    hex "#a1a1a1"


lightGray : Css.Color
lightGray =
    hex "#f5f5f5"


darkGray : Css.Color
darkGray =
    hex "#ccc"


highlightedDay : Css.Color
highlightedDay =
    hex "#ebebeb"


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
        [ borderBoxStyle
        , textAlign center
        , transform (scale 0.8)
        , position absolute
        , padding2 (px 0) (px 8)
        , cursor pointer
        ]


borderBoxStyle : Css.Style
borderBoxStyle =
    Css.batch [ boxSizing borderBox ]


highlightBorderStyle : Css.Style
highlightBorderStyle =
    Css.batch [ borderRadius (px 0) ]


headerStyle : Css.Style
headerStyle =
    Css.batch
        [ padding2 (px 10) (px 7)
        , backgroundColor lightGray
        ]


calendarHeight : Css.Px
calendarHeight =
    px 277


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
        [ backgroundColor (hex "#e0e0e0")
        , highlightBorderStyle
        ]


emptyCellStyle : Css.Style
emptyCellStyle =
    Css.batch [ hover [ backgroundColor unset ], cursor unset ]

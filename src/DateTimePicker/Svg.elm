module DateTimePicker.Svg exposing
    ( doubleLeftArrow
    , doubleRightArrow
    , downArrow
    , leftArrow
    , rightArrow
    , upArrow
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Svg.Styled exposing (Svg, polygon, svg)
import Svg.Styled.Attributes exposing (height, points, style, viewBox, width)


type Orientation
    = Up
    | Down
    | Left
    | Right


type DoubleOrientation
    = DoubleLeft
    | DoubleRight


rightArrow : Svg msg
rightArrow =
    arrow Right


doubleRightArrow : Svg msg
doubleRightArrow =
    doubleArrow DoubleRight


leftArrow : Svg msg
leftArrow =
    arrow Left


doubleLeftArrow : Svg msg
doubleLeftArrow =
    doubleArrow DoubleLeft


downArrow : Svg msg
downArrow =
    arrow Down


upArrow : Svg msg
upArrow =
    arrow Up


arrow : Orientation -> Svg msg
arrow orientation =
    let
        rotation =
            case orientation of
                Right ->
                    "0"

                Left ->
                    "180"

                Down ->
                    "90"

                Up ->
                    "270"
    in
    svg [ width "8", height "12", viewBox "0 0 16 16", style <| "transform: rotate(" ++ rotation ++ "deg);" ]
        [ polygon [ points "0 0, 0 20, 16 10" ] []
        ]


doubleArrow : DoubleOrientation -> Svg msg
doubleArrow orientation =
    let
        rotation =
            case orientation of
                DoubleRight ->
                    "0"

                DoubleLeft ->
                    "180"
    in
    svg [ width "16", height "12", viewBox "0 0 32 16", style <| "transform: rotate(" ++ rotation ++ "deg);" ]
        [ polygon [ points "0 0, 0 20, 16 10" ] []
        , polygon [ points "16 0, 16 20, 32 10" ] []
        ]

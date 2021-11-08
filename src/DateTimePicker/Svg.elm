module DateTimePicker.Svg exposing
    ( doubleLeftArrow
    , doubleRightArrow
    , downArrow
    , leftArrow
    , rightArrow
    , upArrow
    )

{-|


# This is mostly a replacement of <https://github.com/abadi199/datetimepicker/blob/master/src/DateTimePicker/Svg.elm>

-}

import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import Svg.Styled exposing (polygon, svg)
import Svg.Styled.Attributes exposing (height, points, style, viewBox, width)


type DoubleOrientation
    = DoubleLeft
    | DoubleRight


rightArrow : Html msg
rightArrow =
    arrow UiIcon.arrowRight


doubleRightArrow : Html msg
doubleRightArrow =
    doubleArrow DoubleRight


leftArrow : Html msg
leftArrow =
    arrow UiIcon.arrowLeft


doubleLeftArrow : Html msg
doubleLeftArrow =
    doubleArrow DoubleLeft


downArrow : Html msg
downArrow =
    arrow UiIcon.arrowDown


upArrow : Html msg
upArrow =
    arrow UiIcon.arrowTop


arrow : Svg -> Html msg
arrow icon =
    icon
        |> Svg.withWidth (Css.px 12)
        |> Svg.withHeight (Css.px 12)
        |> Svg.toHtml


doubleArrow : DoubleOrientation -> Html msg
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

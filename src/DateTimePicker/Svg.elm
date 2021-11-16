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
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import Svg.Styled exposing (polygon, svg)
import Svg.Styled.Attributes exposing (fill, points, style, viewBox)


type DoubleOrientation
    = DoubleLeft
    | DoubleRight


rightArrow : Svg
rightArrow =
    arrow UiIcon.arrowRight


doubleRightArrow : Svg
doubleRightArrow =
    doubleArrow DoubleRight


leftArrow : Svg
leftArrow =
    arrow UiIcon.arrowLeft


doubleLeftArrow : Svg
doubleLeftArrow =
    doubleArrow DoubleLeft


downArrow : Svg
downArrow =
    arrow UiIcon.arrowDown


upArrow : Svg
upArrow =
    arrow UiIcon.arrowTop


arrow : Svg -> Svg
arrow icon =
    icon
        |> Svg.withWidth (Css.px 12)
        |> Svg.withHeight (Css.px 12)


doubleArrow : DoubleOrientation -> Svg
doubleArrow orientation =
    let
        rotation =
            case orientation of
                DoubleRight ->
                    "0"

                DoubleLeft ->
                    "180"
    in
    svg [ viewBox "0 0 32 16", fill "currentcolor", style <| "transform: rotate(" ++ rotation ++ "deg);" ]
        [ polygon [ points "0 0, 0 20, 16 10" ] []
        , polygon [ points "16 0, 16 20, 32 10" ] []
        ]
        |> Svg.fromHtml
        |> Svg.withWidth (Css.px 16)
        |> Svg.withHeight (Css.px 12)

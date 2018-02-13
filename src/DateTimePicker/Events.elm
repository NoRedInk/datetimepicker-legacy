module DateTimePicker.Events
    exposing
        ( MoveData
        , onBlurWithChange
        , onMouseDownPreventDefault
        , onMouseMoveWithPosition
        , onMouseUpPreventDefault
        , onPointerMoveWithPosition
        , onPointerUp
        , onTouchEndPreventDefault
        , onTouchMovePreventDefault
        , onTouchStartPreventDefault
        )

import Date exposing (Date)
import Html.Styled as Html
import Html.Styled.Events as HtmlEvents exposing (targetValue)
import Json.Decode
import Svg.Styled as Svg
import Svg.Styled.Events as SvgEvents


onBlurWithChange : (String -> Maybe Date) -> (Maybe Date -> msg) -> Html.Attribute msg
onBlurWithChange parser tagger =
    HtmlEvents.on "blur"
        (Json.Decode.map (parser >> tagger) targetValue)


onMouseDownPreventDefault : msg -> Html.Attribute msg
onMouseDownPreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    HtmlEvents.onWithOptions "mousedown" eventOptions (Json.Decode.succeed msg)


onTouchStartPreventDefault : msg -> Html.Attribute msg
onTouchStartPreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    HtmlEvents.onWithOptions "touchstart" eventOptions (Json.Decode.succeed msg)


onMouseUpPreventDefault : msg -> Html.Attribute msg
onMouseUpPreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    HtmlEvents.onWithOptions "mouseup" eventOptions (Json.Decode.succeed msg)


onTouchEndPreventDefault : msg -> Html.Attribute msg
onTouchEndPreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    HtmlEvents.onWithOptions "touchend" eventOptions (Json.Decode.succeed msg)


onMouseMoveWithPosition : (MoveData -> Json.Decode.Decoder msg) -> Svg.Attribute msg
onMouseMoveWithPosition decoder =
    SvgEvents.on "mousemove"
        (mouseMoveDecoder |> Json.Decode.andThen decoder)


onPointerMoveWithPosition : (MoveData -> Json.Decode.Decoder msg) -> Svg.Attribute msg
onPointerMoveWithPosition decoder =
    HtmlEvents.on "pointermove"
        (mouseMoveDecoder |> Json.Decode.andThen decoder)


onPointerUp : msg -> Html.Attribute msg
onPointerUp msg =
    HtmlEvents.on "pointerup" (Json.Decode.succeed msg)


onTouchMovePreventDefault : msg -> Svg.Attribute msg
onTouchMovePreventDefault msg =
    let
        eventOptions =
            { preventDefault = True
            , stopPropagation = True
            }
    in
    HtmlEvents.onWithOptions "touchstart"
        eventOptions
        (Json.Decode.succeed msg)


type alias MoveData =
    { offsetX : Int, offsetY : Int }


mouseMoveDecoder : Json.Decode.Decoder MoveData
mouseMoveDecoder =
    Json.Decode.map2 MoveData
        (Json.Decode.field "offsetX" Json.Decode.float |> Json.Decode.map round)
        (Json.Decode.field "offsetY" Json.Decode.float |> Json.Decode.map round)


touches : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
touches decoder =
    let
        loop idx xs =
            Json.Decode.maybe (Json.Decode.field (toString idx) decoder)
                |> Json.Decode.andThen
                    (Maybe.map (\x -> loop (idx + 1) (x :: xs))
                        >> Maybe.withDefault (Json.Decode.succeed xs)
                    )
    in
    Json.Decode.at [ "touches", "0" ] <| loop 0 []

module DateTimePicker.Events exposing
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

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import DateTimePicker.DateTime as DateTime
import Html.Styled as Html
import Html.Styled.Events as HtmlEvents exposing (targetValue)
import Json.Decode
import Svg.Styled as Svg
import Svg.Styled.Events as SvgEvents


onBlurWithChange : (String -> Maybe DateTime.DateTime) -> (Maybe DateTime.DateTime -> msg) -> Html.Attribute msg
onBlurWithChange parser tagger =
    HtmlEvents.on "blur"
        (Json.Decode.map (parser >> tagger) targetValue)


onMouseDownPreventDefault : msg -> Html.Attribute msg
onMouseDownPreventDefault msg =
    HtmlEvents.custom "mousedown" <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }


onTouchStartPreventDefault : msg -> Html.Attribute msg
onTouchStartPreventDefault msg =
    HtmlEvents.custom "touchstart" <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }


onMouseUpPreventDefault : msg -> Html.Attribute msg
onMouseUpPreventDefault msg =
    HtmlEvents.custom "mouseup" <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }


onTouchEndPreventDefault : msg -> Html.Attribute msg
onTouchEndPreventDefault msg =
    HtmlEvents.custom "touchend" <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }


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
    HtmlEvents.custom "touchstart" <|
        Json.Decode.succeed
            { preventDefault = True
            , stopPropagation = True
            , message = msg
            }


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
            Json.Decode.maybe (Json.Decode.field (String.fromInt idx) decoder)
                |> Json.Decode.andThen
                    (Maybe.map (\x -> loop (idx + 1) (x :: xs))
                        >> Maybe.withDefault (Json.Decode.succeed xs)
                    )
    in
    Json.Decode.at [ "touches", "0" ] <| loop 0 []

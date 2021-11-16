module DateTimePicker.Events exposing
    ( onMouseDownPreventDefault
    , onTouchStartPreventDefault
    )

{-|


# THIS IS A HEAVILY MODIFIED FORK OF <https://github.com/abadi199/datetimepicker>

-}

import Html.Styled as Html
import Html.Styled.Events as HtmlEvents
import Json.Decode


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

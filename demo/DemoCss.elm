module DemoCss exposing (CssClasses(..), css)

import Css exposing (..)


css : Css.Stylesheet
css =
    Css.stylesheet
        []


type CssClasses
    = Container

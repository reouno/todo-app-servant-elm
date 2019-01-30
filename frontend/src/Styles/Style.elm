module Styles.Style exposing (..)

import Css exposing (..)
import Css.Global exposing (..)
import Html
import Html.Styled exposing (..)

myStyle : Html.Html msg
myStyle = toUnstyled (
    global
        [ class "no-display"
            [ display none ]
        ]
    )

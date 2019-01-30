module Main exposing (..)

import Browser exposing (application)

import App.Types exposing (..)
import App.State exposing (..)
import App.View exposing (view)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

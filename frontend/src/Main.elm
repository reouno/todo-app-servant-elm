module Main exposing (..)

import Browser exposing (element)
import Html exposing (..)

type alias Model =
    String

init : () -> (Model, Cmd Msg)
init _ =
    ( "Hello world.", Cmd.none )

type Msg =
    NoOp

view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text model ] ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

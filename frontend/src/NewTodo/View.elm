module NewTodo.View exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Modifiers exposing (..)

import NewTodo.Types exposing (..)
import Todos.Types
import Util.Events exposing (onEnter)

view : Todos.Types.Todo -> Html Msg
view todo =
    let
        myControlAttrs : List (Attribute Msg)
        myControlAttrs = []
        myInputAttrs : List (Attribute Msg)
        myInputAttrs = [ onInput InputNewTodoTitle
                       , onEnter GetCurrentTimeForNewTodo
                       , placeholder "todo"
                       , value todo.title
                       ]
    in
        controlText controlInputModifiers
                     myControlAttrs
                     myInputAttrs
                     []

module App.Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Http
import Url

import NewTodo.Types
import Todos.Types

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , todos : Todos.Types.Todos
    , newTodo : Todos.Types.Todo
    , doneFilter : Todos.Types.DoneFilter
    }

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NewTodoMsg NewTodo.Types.Msg
    | TodosMsg Todos.Types.Msg
    | FetchTodos Todos.Types.DoneFilter
    | NoOp

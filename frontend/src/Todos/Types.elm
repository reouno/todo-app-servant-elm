module Todos.Types exposing (..)

import Http
import Time

type alias TodoId = Int

type alias Todo =
    { id : TodoId
    , title : String
    , done : Bool
    , created : Maybe Time.Posix
    , due : Maybe Time.Posix
    , doneAt : Maybe Time.Posix
    }
type alias Todos = List Todo

type DoneFilter
    = Any
    | Done
    | NotDoneYet

type Msg
    = FetchTodos DoneFilter
    | GotTodos DoneFilter (Result Http.Error Todos)
    | CheckTodo TodoId Bool
    | OnCheckTodo (Result Http.Error Todo)
    | DeleteTodo TodoId
    | OnDeleteTodo TodoId (Result Http.Error ())

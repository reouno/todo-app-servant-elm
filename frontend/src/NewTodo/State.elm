module NewTodo.State exposing (..)

import Return exposing (Return)
import String
import Task
import Time

import Api.TodoApi as TodoApi
import NewTodo.Types exposing (..)
import Todos.State exposing (initialTodo)
import Todos.Types as Todos
import Util.Time exposing (msgWithTime)

update : Msg -> Todos.Todo -> Return Msg Todos.Todo
update msg model =
    case msg of
        InitNewTodo ->
            ( initialTodo, Cmd.none )
        InputNewTodoTitle title ->
            let
                newTodo =
                    { id = 0
                    , title = title
                    , done = model.done
                    , created = model.created
                    , due = model.due
                    , doneAt = model.doneAt
                    }
            in
                ( newTodo, Cmd.none )
        GetCurrentTimeForNewTodo ->
            if String.length model.title == 0 then
                ( model, Cmd.none )
            else
                ( model, msgWithTime CreateNewTodoAfterCurrentTime Time.now )
        CreateNewTodoAfterCurrentTime time ->
            let
                newTodo = { model | created = Just time }
            in
                ( model, TodoApi.postTodo newTodo)
        _ ->
            ( model, Cmd.none )

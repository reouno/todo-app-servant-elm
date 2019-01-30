module Todos.State exposing (..)

import Return exposing (Return)

import Api.TodoApi as TodoApi
import Todos.Types exposing (..)

initialTodos : Todos
initialTodos = []

initialTodo : Todo
initialTodo =
    { title = ""
    , done = False
    , id = 0
    , created = Nothing
    , due = Nothing
    , doneAt = Nothing
    }

update : Msg -> Todos -> Return Msg Todos
update msg model =
    case msg of
        FetchTodos doneFilter ->
            ( model, TodoApi.getTodos doneFilter )
        GotTodos doneFilter result ->
            case result of
                Ok response ->
                    case doneFilter of
                        Any ->
                            ( response, Cmd.none )
                        Done ->
                            ( List.filter (\tds -> tds.done == True) response
                            , Cmd.none )
                        NotDoneYet ->
                            ( List.filter (\tds -> tds.done == False) response
                            , Cmd.none )
                Err error ->
                    ( model, Cmd.none )
        CheckTodo todoId isChecked ->
            let
                maybeTodo = List.head (List.filter (\t -> t.id == todoId) model)
            in
                case maybeTodo of
                    Just todo ->
                        ( model, TodoApi.putTodo { todo | done = isChecked } )
                    Nothing ->
                        ( model, Cmd.none )
        OnCheckTodo result ->
            case result of
                Ok updatedTodo ->
                    let
                        updatedTodos = List.map
                            (\todo -> if todo.id == updatedTodo.id
                                then updatedTodo
                                else todo
                            )
                            model
                    in
                        ( updatedTodos, Cmd.none )
                Err error ->
                    ( model, Cmd.none )
        DeleteTodo todoId ->
            ( model, TodoApi.deleteTodo todoId )
        OnDeleteTodo todoId result ->
            case result of
                Ok () ->
                    let
                        newTodos = List.filter (\todo -> todo.id /= todoId) model
                    in
                        ( newTodos, Cmd.none )
                Err error ->
                    ( model, Cmd.none )

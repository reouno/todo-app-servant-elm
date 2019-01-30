module App.State exposing (..)

import Browser
import Browser.Navigation as Nav
import Return exposing (Return)
import Time
import Url

import Api.TodoApi as Api
import App.Types exposing (..)
import NewTodo.State
import NewTodo.Types
import Todos.State exposing (initialTodos, initialTodo)
import Todos.Types as Todos

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    ( initialModel key url, Cmd.map TodosMsg (Api.getTodos Todos.NotDoneYet ) )

initialModel : Nav.Key -> Url.Url -> Model
initialModel key url =
    { key = key
    , url = url
    , todos = initialTodos
    , newTodo = initialTodo
    , doneFilter = Todos.NotDoneYet
    }

update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key ( Url.toString url ) )
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            ( { model | url = url}, Cmd.none )
        NewTodoMsg (NewTodo.Types.OnCreateNewTodo todo result) ->
            case result of
                Ok todoId ->
                    let
                        newTodo = { todo | id = todoId }
                        newTodos = model.todos ++ [newTodo]
                    in
                        update (NewTodoMsg NewTodo.Types.InitNewTodo) {model | todos=newTodos}
                Err error ->
                    ( model, Cmd.none )
        NewTodoMsg msg_ ->
            let
                (newTodo, cmd) = NewTodo.State.update msg_ model.newTodo
                newModel = { model | newTodo = newTodo }
            in
                ( newModel, Cmd.map NewTodoMsg cmd )
        TodosMsg ( Todos.GotTodos doneFilter result ) ->
            let
                ( todos, cmd ) = Todos.State.update
                                     ( Todos.GotTodos doneFilter result )
                                     model.todos
                newModel = { model | todos = todos, doneFilter = doneFilter }
            in
                ( newModel, Cmd.map TodosMsg cmd )
        TodosMsg msg_ ->
            let
                (todos, cmd) = Todos.State.update msg_ model.todos
                newModel = { model | todos = todos }
            in
                ( newModel, Cmd.map TodosMsg cmd )
        FetchTodos doneFilter ->
            let
                ( todos, cmd ) = Todos.State.update
                    ( Todos.FetchTodos doneFilter )
                    model.todos
            in
                ( model, Cmd.map TodosMsg cmd )
        NoOp ->
            ( model, Cmd.none )

module Api.TodoApi exposing (..)

import Http exposing (..)
import Json.Decode as D
import Json.Decode.Extra as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import Json.Encode.Extra as E

import NewTodo.Types
import Todos.Types as T
import Util.String exposing (joinPath)
import Util.Time exposing (toStringUTC)

todoApiRoot = "http://localhost:3000/api/v0.1"

todoDecoder : D.Decoder T.Todo
todoDecoder =
    D.succeed T.Todo
        |> P.required "id" D.int
        |> P.required "title" D.string
        |> P.required "done" D.bool
        |> P.optional "created" (D.map Just D.datetime) Nothing
        |> P.optional "due" (D.map Just D.datetime) Nothing
        |> P.optional "doneAt" (D.map Just D.datetime) Nothing

todosDecoder : D.Decoder T.Todos
todosDecoder =
    D.list todoDecoder

encodeTodo : T.Todo -> E.Value
encodeTodo x =
    E.object
        [ ( "id", E.int x.id )
        , ( "title", E.string x.title )
        , ( "done", E.bool x.done )
        , ( "created", E.maybe E.string (Maybe.map toStringUTC x.created) )
        , ( "due", E.maybe E.string (Maybe.map toStringUTC x.due) )
        , ( "doneAt", E.maybe E.string (Maybe.map toStringUTC x.doneAt) )
        ]

getTodos : T.DoneFilter -> Cmd T.Msg
getTodos doneFilter =
    Http.get
        { url = joinPath todoApiRoot "todos"
        , expect = Http.expectJson (T.GotTodos doneFilter) todosDecoder
        }

{- ### experiment ###
- using expectJson
-}
postTodo : T.Todo -> Cmd NewTodo.Types.Msg
postTodo todo =
    Http.post
        { url = joinPath todoApiRoot "todo"
        , body = Http.jsonBody (encodeTodo todo)
        , expect = Http.expectJson (NewTodo.Types.OnCreateNewTodo todo) D.int}

{- ### experiment ###
- using expectStringResponse
-}
putTodo : T.Todo -> Cmd T.Msg
putTodo todo =
    Http.request
        { method = "PUT"
        , headers = []
        , url = joinPath todoApiRoot "todo/" ++ String.fromInt todo.id
        , body = jsonBody (encodeTodo todo)
        , expect = Http.expectStringResponse
            (\result -> T.OnCheckTodo result)
            (\response ->
                case response of
                    Http.GoodStatus_ _ body ->
                        if body == String.fromInt todo.id then
                            Ok todo
                        else
                            Err (Http.BadBody
                                "Expected the response body to be the key of the todo")
                    Http.BadUrl_ url ->
                        Err (Http.BadUrl url)

                    Http.Timeout_ ->
                        Err Http.Timeout

                    Http.NetworkError_ ->
                        Err Http.NetworkError

                    Http.BadStatus_ metadata body ->
                        Err (Http.BadStatus metadata.statusCode)
            )
        , timeout = Nothing
        , tracker = Nothing
        }

deleteTodo : T.TodoId -> Cmd T.Msg
deleteTodo todoId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = joinPath todoApiRoot "todo/" ++ String.fromInt todoId
        , body = Http.emptyBody
        , expect = Http.expectWhatever (T.OnDeleteTodo todoId)
        , timeout = Nothing
        , tracker = Nothing
        }

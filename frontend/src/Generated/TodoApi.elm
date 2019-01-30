module Generated.TodoApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import Http.Extra exposing (..)
import String


type alias Todo =
    { title : String
    , done : Bool
    , id : Int
    }

type Msg
    = GotTodos (Result Http.Error (List Todo))
    | GotTodo (Result Http.Error Todo)
    | AddedTodo (Result Http.Error Int)
    | UpdatedTodo (Result Http.Error (Maybe Int))
    | DeletedTodo (Result Http.Error NoContent)


decodeTodo : Decoder Todo
decodeTodo =
    succeed Todo
        |> required "title" string
        |> required "done" bool
        |> required "id" int

encodeTodo : Todo -> Json.Encode.Value
encodeTodo x =
    Json.Encode.object
        [ ( "title", Json.Encode.string x.title )
        , ( "done", Json.Encode.bool x.done )
        , ( "id", Json.Encode.int x.id )
        ]

getApiV0_1Todos : Cmd Msg
getApiV0_1Todos =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "v0_1"
                , "todos"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson GotTodos (list decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getApiV0_1TodoById : Int -> Cmd Msg
getApiV0_1TodoById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "v0_1"
                , "todo"
                , capture_id |> String.fromInt
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson GotTodo (maybe decodeTodo)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postApiV0_1Todo : Todo -> Cmd Msg
postApiV0_1Todo body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "v0_1"
                , "todo"
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectJson AddedTodo int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putApiV0_1TodoById : Int -> Todo -> Cmd Msg
putApiV0_1TodoById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "v0_1"
                , "todo"
                , capture_id |> String.fromInt
                ]
        , body =
            Http.jsonBody (encodeTodo body)
        , expect =
            Http.expectJson UpdatedTodo (maybe int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteApiV0_1TodoById : Int -> Cmd Msg
deleteApiV0_1TodoById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "api"
                , "v0_1"
                , "todo"
                , capture_id |> String.fromInt
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse DeletedTodo
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

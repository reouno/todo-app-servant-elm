module Todos.View exposing (..)

import Bulma.Columns exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onCheck, onClick)

import Styles.BulmaModifiers exposing (..)
import Todos.Types exposing (..)

view : Todos -> Html Msg
view todos =
    let
        --notDoneYet = List.filter (\t -> t.done == False) todos
        notDoneYet = todos
    in
        div []
            (List.map viewTodo notDoneYet)

viewTodo : Todo -> Html Msg
viewTodo todo =
    div [ class "m1" ]
        [ columns todoColsModifiers []
            [ column checkColModifiers []
                [ checkButton todo
                ]
            , column mainColModifiers []
                [ showTodo todo
                ]
            , column delColModifiers []
                [ deleteBtn todo
                ]
            ]
        ]

checkButton : Todo -> Html Msg
checkButton todo =
    div []
        [ controlCheckBox False
            []
            []
            [ checked todo.done
            , onCheck (GetCurrentTimeForCheckTodo todo.id)
            ]
            []
        ]

showTodo : Todo -> Html Msg
showTodo todo =
    p [] [ text todo.title ]

--deleteBtn : Todo -> Html Msg
--deleteBtn todo =
--    p
--        [ class "clearfix right control" ]
--        [ Html.button
--            [ class "button is-danger is-small"
--            , onClick ( DeleteTodo todo.id )
--            ]
--            [ text "Del" ]
--        ]

deleteBtn : Todo -> Html Msg
deleteBtn todo =
    controlInput delBtnModifiers
        []
        [ type_ "button"
        , class "button"
        , onClick ( DeleteTodo todo.id )
        , value "Del"
        ]
        []

--deleteBtn_ : Todo -> Html Msg
--deleteBtn_ todo =
--    controlButton delBtnModifiers
--        [ class "clearfix right" ]
--        [ onClick ( DeleteTodo todo.id ) ]
--        [ text "Del" ]

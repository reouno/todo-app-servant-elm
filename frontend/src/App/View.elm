module App.View exposing (..)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Modifiers exposing (..)
import Bulma.Elements as B
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Layout exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import App.Types exposing (..)
import NewTodo.View
import Styles.BulmaModifiers exposing (..)
import Styles.Style exposing (..)
import Todos.Types as Todos
import Todos.View

view : Model -> Browser.Document Msg
view model =
    { title = "Todo App by Elm and Servant"
    , body = viewMain model
    }

viewMain : Model -> List (Html Msg)
viewMain model =
    [ div []
        [ stylesheet
        , viewHeader model
        , viewBody model
        , viewFooter model
        ]
    ]

grid : Model -> Html Msg
grid model =
    columns columnsModifiers []
        [ column columnModifiers []
            [ mainColHeader model
            , mainColBody model
            , mainColFooter model
            ]
        ]

viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "bg-silver" ]
        [ h2 [] [ text "Awesome Todo App" ] ]

--viewHeader : Model -> Html Msg
--viewHeader model =
--    hero headerModifiers
--        []
--        [ heroBody []
--            [ container []
--                [ B.title B.H3 [] [ text "Awesome Todo App" ]
--                ]
--            ]
--        ]

viewBody : Model -> Html Msg
--viewBody model = grid model
viewBody model =
    div [ class "m1" ]
        [ viewTabs model
        , viewTabContents model
        ]

viewTabs : Model -> Html Msg
viewTabs model =
    div []
        [ input [ id "tab1", type_ "radio", name "tab_btn"
                , class "display-none"
                , checked (model.doneFilter == Todos.NotDoneYet)
                , onClick ( FetchTodos Todos.NotDoneYet ) ] []
        , input [ id "tab2", type_ "radio", name "tab_btn"
                , class "display-none"
                , checked (model.doneFilter == Todos.Done)
                , onClick ( FetchTodos Todos.Done ) ] []
        , div []
            [ label [ class "btn", for "tab1" ] [ text "Todo" ]
            , label [ class "btn", for "tab2" ] [ text "Done" ]
            ]
        ]

viewTabContents : Model -> Html Msg
viewTabContents model =
    grid model

viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "m1" ]
        [ p [] [ text "© 2019 Fictitious Company, Inc." ]
        , myStyle ]

mainColHeader : Model -> Html Msg
mainColHeader model =
    div [] []

mainColBody : Model -> Html Msg
mainColBody model =
    div []
        [ Html.map TodosMsg (Todos.View.view model.todos)
        , Html.map NewTodoMsg (NewTodo.View.view model.newTodo)
        ]

mainColFooter : Model -> Html Msg
mainColFooter model =
    div [] []


---------- Experiments ----------

exampleHero : Html Msg
exampleHero
  = hero { heroModifiers | size = Medium, color = Primary } []
    [ heroBody []
      [ container []
          [ B.title B.H1 [] [ text "Hero Title" ]
          , B.title B.H2 [] [ text "Hero Subtitle" ]
          ]
      ]
    ]

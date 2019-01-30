module NewTodo.Types exposing (..)

import Http
import Time

import Todos.Types exposing (Todo, TodoId)

type Msg
    = InitNewTodo
    | InputNewTodoTitle String
    | GetCurrentTimeForNewTodo
    | CreateNewTodoAfterCurrentTime Time.Posix
    | OnCreateNewTodo Todo (Result Http.Error TodoId)

module Handlers.TodoHandler where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Servant.API (NoContent(..))

import Models

getTodos :: ConnectionPool ->  IO [Entity Todo]
getTodos pool = flip runSqlPool pool $ selectList [] []

getTodo :: ConnectionPool -> Key Todo -> IO (Maybe Todo)
getTodo pool id' = flip runSqlPool pool $ get id'

addTodo :: ConnectionPool -> Todo -> IO (Key Todo)
addTodo pool todo = flip runSqlPool pool $ insert todo

replaceTodo :: ConnectionPool -> Key Todo -> Todo -> IO (Maybe (Key Todo))
replaceTodo pool id' todo = flip runSqlPool pool $ do
    exist <- get id'
    if isNothing exist then
        return Nothing
    else do
        replace id' todo
        return $ Just id'

-- deleteTodo :: (MonadIO a, MonadUnliftIO a) => ConnectionPool -> Key Todo -> a NoContent
deleteTodo :: ConnectionPool -> Key Todo -> IO ()
deleteTodo pool id' = flip runSqlPool pool $ delete id'

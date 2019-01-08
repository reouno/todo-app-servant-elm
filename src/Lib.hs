{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Lib where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (isJust)
import qualified Data.Map as M
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Server (serve)
import System.FilePath
import Web.Internal.FormUrlEncoded (FromForm)

type TodoId = Int

data Todo = Todo
    { title :: String
    , done :: Bool
    } deriving (Eq, Generic, Show)

instance FromForm Todo
instance FromJSON Todo
instance ToJSON Todo

data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
    mimeRender _ bs = bs

type TodoAPI =
    "api" :> "todos" :> Get '[JSON] TodoMap
    :<|> "api" :> "todo" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[JSON] TodoId
    :<|> "api" :> "todo" :> Capture "id" Int :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] TodoId
    :<|> "api" :> "todo" :> Capture "id" Int :> Delete '[JSON] NoContent

type API =
    TodoAPI
    :<|> Get '[HTML] ByteString
    :<|> Raw

api :: Proxy API
api = Proxy

apiServer :: DB -> Server TodoAPI
apiServer db =
    getTodos db
    :<|> postTodo db
    :<|> putTodo db
    :<|> deleteTodo db
    where
        getTodos :: DB -> Handler TodoMap
        getTodos db = liftIO $ selectAllWithId db
        postTodo :: DB -> Todo -> Handler TodoId
        postTodo db todo = liftIO $ insert db todo
        putTodo :: DB -> TodoId -> Todo -> Handler TodoId
        putTodo db id' todo = liftIO $ update db id' todo
        deleteTodo :: DB -> TodoId -> Handler NoContent
        deleteTodo = delete

root :: String
root = "www/dist"

server :: IO (Server API)
server = do
    db <- mkDB
    indexHtml <- B.readFile $ root </> "index.html"
    let
        server' =
            apiServer db
            :<|> pure indexHtml
            :<|> serveDirectoryWebApp root
    return server'

app ::  IO Application
app = serve api <$> server

type TodoMap = M.Map TodoId Todo
newtype DB = DB (TVar (Int, TodoMap))

mkDB :: IO DB
mkDB = DB <$> newTVarIO (0, M.empty)

selectAll :: DB -> IO [Todo]
selectAll (DB db) = M.elems . snd <$> readTVarIO db

selectAllWithId :: DB -> IO TodoMap
selectAllWithId (DB db) = snd <$> readTVarIO db

select :: DB -> TodoId -> IO (Maybe Todo)
select (DB db) id' = M.lookup id' . snd <$> readTVarIO db

insert :: DB -> Todo -> IO TodoId
insert (DB db) todo = do
    (maxId, todoMap) <- readTVarIO db
    let newId = maxId + 1
    atomically $ writeTVar db (newId, M.insert newId todo todoMap)
    --atomically $ modifyTVar db \(maxId, todoMap) ->
    --    let newId = maxId + 1
    --    in (newId, M.insert newId todo todoMap)
    return newId

update :: DB -> TodoId -> Todo -> IO TodoId
update (DB db) id' todo = do
    (maxId, todoMap) <- readTVarIO db
    if isJust (M.lookup id' todoMap) then do
        atomically $ writeTVar db (maxId, M.update (\_ -> Just todo) id' todoMap)
        return id'
    else
        error "no such id!" -- should be replaced by `err404`

delete :: MonadIO m => DB -> TodoId -> m NoContent
delete (DB db) id' = liftIO $ do
    (maxId, todoMap) <- readTVarIO db
    atomically $ writeTVar db (maxId, M.delete id' todoMap)
    return NoContent

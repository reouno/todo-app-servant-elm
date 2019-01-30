{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.Server (serve)
import System.FilePath

import Config.Config (_database_, _distDir_, _entry_)
import DataStore.Internal
import qualified Apis.TodoApi as TA
import qualified Handlers.TodoHandler as TH

data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
    mimeRender _ bs = bs

type API =
    TA.TodoAPI
    :<|> Get '[HTML] ByteString
    :<|> Raw

api :: Proxy API
api = Proxy

todoApiServer :: ConnPool -> Server TA.TodoAPI
todoApiServer pool =
    getTodos pool
    :<|> getTodo pool
    :<|> postTodo pool
    :<|> putTodo pool
    :<|> deleteTodo pool
    where
        getTodos pool = liftIO $ TH.getTodos pool
        getTodo pool id' = liftIO $ TH.getTodo pool id'
        postTodo pool todo = liftIO $ TH.addTodo pool todo
        putTodo pool id' todo = liftIO $ TH.replaceTodo pool id' todo
        deleteTodo pool id' = liftIO $ TH.deleteTodo pool id'

root :: String
root = _distDir_

server :: IO (Server API)
server = do
    pool <- mkPool _database_
    doMigration pool
    indexHtml <- B.readFile $ root </> _entry_
    let
        server' =
            todoApiServer pool
            :<|> pure indexHtml
            :<|> serveDirectoryWebApp root
    return server'

app ::  IO Application
app = serve api <$> server

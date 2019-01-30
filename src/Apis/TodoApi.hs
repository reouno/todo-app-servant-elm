{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Apis.TodoApi where

import Database.Persist
import Models (Todo(..))
import Servant

type TodoAPI =
    "api" :> "v0.1" :> "todos" :> Get '[JSON] [Entity Todo]
    :<|> "api" :> "v0.1" :> "todo" :> Capture "id" (Key Todo) :> Get '[JSON] (Maybe Todo)
    :<|> "api" :> "v0.1" :> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] (Key Todo)
    :<|> "api" :> "v0.1" :> "todo" :> Capture "id" (Key Todo) :> ReqBody '[JSON] Todo :> Put '[JSON] (Maybe (Key Todo))
    :<|> "api" :> "v0.1" :> "todo" :> Capture "id" (Key Todo) :> Delete '[JSON] ()

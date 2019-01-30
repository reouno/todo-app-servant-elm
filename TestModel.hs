{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Database.Persist
import Database.Persist.Sqlite

import Models

main :: IO ()
main = runSqlite ":memory:" $ do
    time <- liftIO getCurrentTime
    liftIO $ print time
    runMigration migrateAll
    newId <- insert $ Todo "new todo" False time Nothing Nothing
    newId2 <- insert $ Todo "without timestamp" False Nothing Nothing Nothing
    row <- get newId
    liftIO $ print row
    return ()

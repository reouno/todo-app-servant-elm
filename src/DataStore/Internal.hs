module DataStore.Internal where

import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions (cs)
import Database.Persist.Sql
import Database.Persist.Sqlite

import Models

type ConnPool = ConnectionPool

mkPool :: FilePath -> IO ConnPool
mkPool filePath = runStderrLoggingT $ createSqlitePool (cs filePath) 5

doMigration :: ConnPool -> IO ()
doMigration = runSqlPool (runMigration migrateAll)

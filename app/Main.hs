{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import RIO.Process
import Environment
import Server
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  let stageFromEnv = stageFromPC pc
  let portFromEnv = portFromPC pc
  myConnectionPool <- initConnectionPool (databaseUrlFromPC pc)
  withLogFunc logOptions $ \lf ->
    let app = Env
          { _appLogFunc = lf
          , _connectionPool = myConnectionPool
          , _stage = stageFromEnv
          , _port = portFromEnv
          }
     in runRIO app startServer
    
initConnectionPool :: DatabaseUrl -> IO (Pool Connection)
initConnectionPool (DatabaseUrl connStr) = createPool (connectPostgreSQL connStr) close 2 60 5
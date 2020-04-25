{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import RIO.Process
import Environment
import Server

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  let stageFromEnv = stageFromPC pc
  let portFromEnv = portFromPC pc
  withLogFunc logOptions $ \lf ->
    let app = Env
          { _appLogFunc = lf
          , _stage = stageFromEnv
          , _port = portFromEnv
          }
     in runRIO app startServer
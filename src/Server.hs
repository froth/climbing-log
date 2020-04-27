{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}

module Server
  ( startServer,
  )
where

import Control.Monad.Except (ExceptT(..))
import Import
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.ForceSSL
import Servant
import Data.Aeson
import Grades

startServer :: RIO Env ()
startServer = do
  localPort <- view port
  theStage <- view stage
  logInfo $ "Stage: " <> displayShow theStage
  logInfo $ "port" <> displayShow localPort
  application <- buildApp
  liftIO $ run localPort application

buildApp :: (WithStage env) => RIO env Application
buildApp = do
  localstage <- view stageL
  env <- ask
  return . sslRedirect localstage . serve api . hoist $ env

data Ascent a
  = Ascent
      { grade :: Grade a,
        date :: Text
      } deriving (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

type API = "ascents" :> Get '[JSON] [Ascent French]

api :: Proxy API
api = Proxy

sslRedirect :: Stage -> Middleware
sslRedirect Dev = id
sslRedirect Prod = forceSSL

hoist :: forall env . env -> Server API
hoist env = hoistServer api nat server
  where nat :: RIO env a -> Servant.Handler a
        nat act = Servant.Handler $ ExceptT $ try $ runRIO env act

server :: ServerT API (RIO env)
server = return ascents

ascents :: [Ascent French]
ascents = [
    Ascent (Grade F7a) "1.1.1970",
    Ascent (Grade F6b) "1.1.1980"
    ]
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import API
import Ascents

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
  return . sslRedirect localstage . serveWithContext api basicAuthServerContext . hoist $ env

api :: Proxy API
api = Proxy

sslRedirect :: Stage -> Middleware
sslRedirect Dev = id
sslRedirect Prod = forceSSL

hoist :: forall env . env -> Server API
hoist env = hoistServerWithContext api ctx nat server
  where nat :: RIO env a -> Servant.Handler a
        nat act = Servant.Handler $ ExceptT $ try $ runRIO env act
        ctx :: Proxy '[BasicAuthCheck User]
        ctx = Proxy

server :: ServerT API (RIO env)
server _ = return ascents

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (User "servant"))
        else return Unauthorized
  in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext
{-# LANGUAGE DataKinds #-}

module Server
  ( startServer,
  )
where

import Control.Monad.Except (ExceptT(..))
import Import
import EnvTypes
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.ForceSSL
import Servant
import API
import Ascents
import Auth

startServer :: RIO Env ()
startServer = do
  localPort <- view port
  theStage <- view stage
  logInfo $ "Stage: " <> displayShow theStage
  logInfo $ "port" <> displayShow localPort
  application <- buildApp
  liftIO $ run localPort application

buildApp :: (WithConnectionPool env, WithStage env) => RIO env Application
buildApp = do
  localstage <- view stageL
  env <- ask
  return . sslRedirect localstage . serveWithContext api (basicAuthServerContext env) . hoist $ env

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


basicAuthServerContext :: WithConnectionPool env => env -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext env = (authCheck env) :. EmptyContext
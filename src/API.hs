{-# LANGUAGE DataKinds #-}

module API where

import Import

import Servant

type API = "ascents" :> BasicAuth "routenbuch" User :> Get '[JSON] [Ascent]
type API2 = "ascents" :> Get '[JSON] [Ascent]

api :: Proxy API
api = Proxy

api2 :: Proxy API2
api2 = Proxy
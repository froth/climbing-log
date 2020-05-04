{-# LANGUAGE DataKinds #-}

module API where

import Import
import Grades

import Servant

type API = "ascents" :> BasicAuth "routenbuch" User :> Get '[JSON] [Ascent French]
type API2 = Get '[JSON] [Ascent French]

api :: Proxy API
api = Proxy

api2 :: Proxy API2
api2 = Proxy
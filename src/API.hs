{-# LANGUAGE DataKinds #-}

module API where

import Import
import Grades

import Servant

type API = "ascents" :> BasicAuth "routenbuch" User :> Get '[JSON] [Ascent French]
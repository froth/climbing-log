{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import RIO

import Lens.Micro.TH


data Stage = Prod | Dev
  deriving (Eq, Read, Show)

data Env = Env
  { _appLogFunc :: !LogFunc
  , _stage :: !Stage
  , _port :: !Int
  -- Add other app-specific configuration information here
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = appLogFunc


class WithStage env where
  stageL :: Lens' env Stage

instance WithStage Env where
  stageL = stage

class WithPort env where
  portL :: Lens' env Int

instance WithPort Env where
  portL = port

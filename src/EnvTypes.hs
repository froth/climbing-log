{-# LANGUAGE TemplateHaskell #-}
module EnvTypes where

import RIO
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
import Lens.Micro.TH

newtype DatabaseUrl = DatabaseUrl ByteString

data Stage = Prod | Dev
  deriving (Eq, Read, Show)

data Env = Env
  { _appLogFunc :: !LogFunc
  , _connectionPool :: !(Pool Connection)
  , _stage :: !Stage
  , _port :: !Int
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = appLogFunc

class WithConnectionPool env where
  connectionPoolL :: Lens' env (Pool Connection)

instance WithConnectionPool Env where
  connectionPoolL = connectionPool

class WithStage env where
  stageL :: Lens' env Stage

instance WithStage Env where
  stageL = stage

class WithPort env where
  portL :: Lens' env Int

instance WithPort Env where
  portL = port

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Types where

import RIO
import Grades
import Data.Aeson
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Lens.Micro.TH

newtype DatabaseUrl = DatabaseUrl ByteString

data Stage = Prod | Dev
  deriving (Eq, Read, Show)

data Env = Env
  { _appLogFunc :: !LogFunc
  , _connectionPool :: !(Pool Connection)
  , _stage :: !Stage
  , _port :: !Int
  -- Add other app-specific configuration information here
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

data Ascent a
  = Ascent
      { grade :: Grade a,
        date :: Text
      } deriving (Generic, Eq, Show)
    deriving anyclass (FromJSON, ToJSON)

newtype Email = Email Text deriving newtype (FromField)

newtype Password = Password Text

newtype PasswordHash = PasswordHash ByteString deriving newtype (FromField)

data User = User {
  userId :: Int,
  email :: Email,
  pwHash :: PasswordHash
} deriving (Generic, FromRow)

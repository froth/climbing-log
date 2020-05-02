{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Types where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Grades
import RIO

data Ascent a
  = Ascent
      { grade :: Grade a,
        date :: Text
      }
  deriving (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Email = Email Text deriving (Show, Eq)
  deriving newtype (FromField)

newtype Password = Password Text deriving (Show, Eq)

newtype PasswordHash = PasswordHash ByteString deriving (Show, Eq)
  deriving newtype (FromField)

data User
  = User
      { userId :: Int,
        email :: Email,
        pwHash :: PasswordHash
      }
  deriving (Show, Generic, FromRow, Eq)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types where

import RIO
import Grades
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField

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

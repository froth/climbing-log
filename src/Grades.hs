{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Grades where

import RIO
import RIO.Partial
import Data.Aeson

newtype Grade a = Grade a deriving (Generic, Eq, Show)
    deriving newtype (FromJSON, ToJSON)

instance Functor Grade where
  fmap f (Grade a) = Grade $ f a

data UIAA = UVII | UVIIp | UVIIpVIIIm | UVIIIm | UVIII deriving (Show, Enum, Eq)

data French = F6b | F6bp | F6c | F6cp | F7a deriving (Show, Enum, Eq, Generic)

instance ToJSON French where

data Saxony = SVIIIb | SVIIIc | SIXa | SIXb deriving (Show, Enum, Eq, Ord)

class GradeConvert a b where
  convert :: a -> b

instance  GradeConvert UIAA French where
  convert = toEnum . fromEnum

instance GradeConvert Saxony UIAA where
  convert s
    | s <= SVIIIc = toEnum sEnum
    | otherwise = toEnum (sEnum + 1)
    where
      sEnum = fromEnum s

saxtoFrench :: Saxony -> French
saxtoFrench sax = convert uiaa
  where uiaa :: UIAA
        uiaa = convert sax
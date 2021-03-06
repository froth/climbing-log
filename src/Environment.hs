module Environment where

import Import
import EnvTypes
import qualified RIO.Map as Map

import qualified RIO.Text as T
import RIO.Process


portFromPC :: ProcessContext -> Int
portFromPC pc = 
  fromMaybe 8080 $ maybePort >>= (readMaybe . T.unpack)
  where 
    maybePort = envFromPC pc "PORT"

stageFromPC :: ProcessContext -> Stage
stageFromPC pc =
   fromMaybe Dev $ maybeStage >>= (readMaybe . T.unpack)
   where 
     maybeStage = envFromPC pc "STAGE"
  
envFromPC :: ProcessContext -> Text -> Maybe Text
envFromPC pc key = Map.lookup key envVars
  where
    envVars = view envVarsL pc

databaseUrlFromPC :: ProcessContext -> DatabaseUrl
databaseUrlFromPC pc =
   DatabaseUrl . encodeUtf8 . fromMaybe "postgresql://postgres@localhost" $ maybeUrl
   where 
     maybeUrl = envFromPC pc "DATABASE_URL"
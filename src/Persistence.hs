{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Persistence where

import Data.Pool
import Database.PostgreSQL.Simple
import Import

getUser :: WithConnectionPool env => Email -> RIO env (Maybe User)
getUser (Email mail) = do 
    pool <- view connectionPoolL
    liftIO (getUser' pool)
    where 
        getUser' pool = withResource pool $ \conn -> do
            users <- query conn "SELECT * FROM users where email = (?)" (Only mail)
            return $ listToMaybe users
    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Persistence where

import Data.Pool
import Database.PostgreSQL.Simple
import Import

getUser :: WithConnectionPool env => ByteString -> RIO env (Maybe User)
getUser mail = do 
    pool <- view connectionPoolL
    liftIO (getUser' pool)
    where 
        getUser' pool = withResource pool $ \conn -> do
            users <- query conn "SELECT * FROM users where email = (?)" (Only mail)
            return $ listToMaybe users
    
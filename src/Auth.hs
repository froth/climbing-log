{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth where

import Crypto.BCrypt
import Import
import Persistence
import Servant

checkpassword :: Maybe User -> ByteString -> BasicAuthResult User
checkpassword Nothing _ = Unauthorized
checkpassword (Just user) pw =
  if validatePassword hash pw
    then Authorized user
    else Unauthorized
  where
    PasswordHash hash = pwHash user

authCheck :: WithConnectionPool env => env -> BasicAuthCheck User
authCheck env = BasicAuthCheck $ \case
  (BasicAuthData username password) -> do
    user <- runRIO env $ getUser (Email . decodeUtf8Lenient $ username)
    return $ checkpassword user password

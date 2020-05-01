{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth where

import Crypto.BCrypt
import Import
import Persistence
import Servant

checkpassword :: Maybe User -> Password -> BasicAuthResult User
checkpassword Nothing _ = Unauthorized
checkpassword (Just user) (Password pw) =
  if validatePassword hash pwByteString
    then Authorized user
    else Unauthorized
  where
    PasswordHash hash = pwHash user
    pwByteString = encodeUtf8 pw

authCheck :: WithConnectionPool env => env -> BasicAuthCheck User
authCheck env = BasicAuthCheck $ \case
  (BasicAuthData username password) -> do
    user <- runRIO env $ getUser (Email . decodeUtf8Lenient $ username)
    return $ checkpassword user (Password . decodeUtf8Lenient $ password)

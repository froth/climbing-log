{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth where

import Import
import Crypto.BCrypt
import Servant
import Persistence

checkpassword :: Maybe User -> ByteString -> BasicAuthResult User
checkpassword Nothing _ = Unauthorized
checkpassword (Just user) pw =
  if validatePassword hash pw
    then Authorized user
    else Unauthorized
  where hash = pwHash user

checkUser :: WithConnectionPool env => env -> BasicAuthData -> IO (BasicAuthResult User)
checkUser env (BasicAuthData username password) = do
  user <- runRIO env (getUser username)
  return $ checkpassword user password

authCheck :: WithConnectionPool env => env -> BasicAuthCheck User
authCheck env = BasicAuthCheck (checkUser env)
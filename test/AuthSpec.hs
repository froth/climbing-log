module AuthSpec where

import RIO
import RIO.Partial
import Auth
import Servant

import Test.Hspec
import Types
import Crypto.BCrypt

testUser :: ByteString -> User
testUser hash = User 1 (Email "testmail") (PasswordHash hash)

spec :: Spec
spec =
  describe "Auth" $ do
  it "fails when no user is provided" $
      checkpassword Nothing (Password "pw") `shouldBe` Unauthorized
  it "fails when a user is provided but the hash is incorrect" $ do
      let pw = "WrongPW"
      hash <- hashPasswordUsingPolicy fastBcryptHashingPolicy "RealPW"
      checkpassword (testUser <$> hash) (Password pw) `shouldBe` Unauthorized
  it "succeeds when a user is provided but the hash is incorrect" $ do
      let pw = "PW"
      hash <- hashPasswordUsingPolicy fastBcryptHashingPolicy "PW"
      let user = testUser <$> hash
      checkpassword user (Password pw) `shouldBe` Authorized (fromJust user)

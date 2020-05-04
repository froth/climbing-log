module ApiDocGenerator where

import API
import Data.Aeson.Encode.Pretty (encodePretty)
import RIO
import qualified RIO.ByteString.Lazy as BL
import Servant.Swagger
import Data.Swagger

-- TODO use haskell-servant - swagger

climbingSwagger :: Swagger
climbingSwagger = toSwagger api2

main :: IO ()
main = BL.writeFile "swagger.json" (encodePretty climbingSwagger)

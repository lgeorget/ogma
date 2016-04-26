module Main where

import qualified Ogma.Api            as Api
import           Ogma.Api.Definition
import           Servant
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Network.Wai.Handler.Warp

ogmaProxy :: Proxy OgmaApi
ogmaProxy = Proxy

ogma :: Application
ogma = simpleCors $ serve ogmaProxy Api.hello

main :: IO ()
main = run 8080 ogma

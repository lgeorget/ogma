{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Ogma.Api.Definition
import Reflex.Dom
import Servant.API
import Servant.Reflex
import Data.Maybe
import Data.Text (Text, unpack)

ogmaProxy :: Proxy OgmaApi
ogmaProxy = Proxy

getHello :: forall t m.MonadWidget t m
         => Event t ()
         -> m (Event t (Maybe Hello, XhrResponse))
getHello = client (Proxy :: Proxy OgmaApi)
                  (Proxy :: Proxy m)
                  (constDyn $ BaseUrl Http "localhost" 8080 "/")

gui :: forall t m.MonadWidget t m
    => m ()
gui = do
  el "div" $ do
    ev <- button "test"
    hi <- fmap fst <$> getHello ev
    let
      e :: Event t String
      e = maybe "err" (unpack . hello) `fmap` hi
    holdDyn "nop" e >>= dynText

main :: IO ()
main = mainWidget gui


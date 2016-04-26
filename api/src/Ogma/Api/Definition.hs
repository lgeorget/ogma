{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Ogma.Api.Definition 
Copyright   : (c) Ogma Project, 2016
License     : MIT
Stability   : experimental

-}

module Ogma.Api.Definition where

import           GHC.Generics
import           Data.Text   (Text)
import           Servant.API
import           Data.Aeson

data Hello = Hello { hello :: Text }
  deriving (Generic)

instance ToJSON Hello
instance FromJSON Hello

type OgmaApi = "hello" :> Get '[JSON] Hello

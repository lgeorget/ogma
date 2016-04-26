{-# LANGUAGE OverloadedStrings #-}

module Ogma.Api where

import           Servant
import           Data.Text (Text)

import           Ogma.Api.Definition

hello :: Server OgmaApi 
hello = return $ Hello "test"

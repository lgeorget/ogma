{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ogma.Model.Privilege where

import Database.Persist.TH
import Data.String

data Privilege = None | ReadOnly | Edit
    deriving (Show, Read, Eq, Ord)
derivePersistField "Privilege"

stringify :: IsString a
          => Privilege
          -> a
stringify None     = "--"
stringify ReadOnly = "r-"
stringify Edit     = "rw"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Ogma.Model.Privilege where

import           Data.String
import           Database.Persist.TH

data Privilege = None | ReadOnly | Edit
    deriving (Show, Read, Eq, Ord)
derivePersistField "Privilege"

stringify :: IsString a
          => Privilege
          -> a
stringify None     = "--"
stringify ReadOnly = "r-"
stringify Edit     = "rw"

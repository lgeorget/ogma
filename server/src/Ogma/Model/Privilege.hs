{-# LANGUAGE TemplateHaskell #-}
module Ogma.Model.Privilege where

import Database.Persist.TH

data Privilege = None | ReadOnly | Edit
    deriving (Show, Read, Eq)
derivePersistField "Privilege"

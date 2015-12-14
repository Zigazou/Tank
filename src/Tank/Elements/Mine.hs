{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Mine
Description : Something that stays in place and explode when anything touches it
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A `Mine` is something that stays in place and explode when anything comes to
touch it.
-}
module Tank.Elements.Mine
( Mine (..)
  -- * Mine lenses
, minePos
) where

import Control.Lens
import Tank.Units

{-|
A `Mine` is an ammunition that can be buried somewhere.
-}
data Mine = Mine { _minePos :: Coords } deriving (Eq, Show, Read)

makeLenses ''Mine
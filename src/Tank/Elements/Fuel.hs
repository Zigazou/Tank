{- |
Module      : Fuel
Description : Fuel
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Elements.Fuel (Fuel (Fuel)) where

{-|
`Fuel` is simply… `Fuel`! It is meant to be used inside a `Reservoir`.
-}
data Fuel = Fuel deriving (Eq, Ord, Show, Read)

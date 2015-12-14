{- |
Module      : Tank.Units
Description : Units making life easier
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Simple types making life easier:

- `Angle`, handles unit of an angle in either degree or radians and makes
  operations agnostics of the unit
- `Coords`, handles coordinates in either polar or rectangular representation and
  makes operations agnostics of the unit
- a `Reservoir` can contain any item, keep track of the remaining items and
  handles state making it easy to do operations like a takeout
-}
module Tank.Units
( module Tank.Units.Angle 
, module Tank.Units.Coords 
, module Tank.Units.Reservoir 
) where

import Tank.Units.Angle 
import Tank.Units.Coords 
import Tank.Units.Reservoir 

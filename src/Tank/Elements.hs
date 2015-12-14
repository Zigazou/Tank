{- |
Module      : Tank.Elements
Description : Elements of the game
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Contains all elements used in the game:

- `Fuel`, needed to make a `Tank` go
- `Mine` which stays at a specific place when dropped
- `Missile` which goes in a direction after being fired
- `Tank` which contains `Fuel`, `Mine` and `Missile`.
-}
module Tank.Elements
( module Tank.Elements.Fuel
, module Tank.Elements.Mine
, module Tank.Elements.Missile
, module Tank.Elements.Tank
) where

import Tank.Elements.Fuel
import Tank.Elements.Mine
import Tank.Elements.Missile
import Tank.Elements.Tank

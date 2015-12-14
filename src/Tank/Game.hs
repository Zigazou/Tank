{- |
Module      : Tank.Game
Description : Game state and rules
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handles the game itself:

- the `Playfield`
- `Action` that can be applied to a `Playfield`
- and the `Engine` managing the game rules.
-}
module Tank.Game
( module Tank.Game.Action
, module Tank.Game.Engine
, module Tank.Game.Playfield
) where

import Tank.Game.Action
import Tank.Game.Engine
import Tank.Game.Playfield

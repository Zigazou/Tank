{- |
Module      : Action
Description : Available actions to a player
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Game.Action
( Action (Forward, Backward, TurnCat, TurnTur, Fire, DropMine, DoNothing)
) where

import Tank.Units
import Tank.Game.Playfield

{-|
Authorized `Action` are among the following.
-}
data Action = Forward TankID -- ^ Make the `Tank` go forward
            | Backward TankID -- ^ Make the `Tank` go backward
            | TurnCat TankID Angle -- ^ Turn the caterpillar
            | TurnTur TankID Angle -- ^ Turn the turret
            | Fire TankID -- ^ Fire a `Missile`
            | DropMine TankID -- ^ Drop a `Mine` where the `Tank` stands
            | DoNothing -- ^ Do nothing
            deriving (Eq, Show)

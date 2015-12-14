{- |
Module      : Engine
Description : The engine playing a game on a `Playfield`
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The engine handles the rules of the game:

- apply `Action`
- manage out of bounds elements
- designate the winner

-}
module Tank.Game.Engine
( Winner (..)
, engine
) where

import Control.Lens
import Control.Monad.State.Lazy

import Tank.Units
import Tank.Elements
import Tank.Game.Playfield
import Tank.Game.Action

{-|
Designates who is the winner, if any.
-}
data Winner = NoWinner -- ^ No winner, the game may go on
            | DrawGame -- ^ No winner but it’s a draw game
            | Winner TankID -- ^ Someone won!

{-|
Given a `Playfield` and an `Action`, returns the updated `Playfield`.
-}
engine :: Playfield -> Action -> Playfield
engine playfield action = execState (engineS action) playfield

{-|
The game itself.
-}
engineS :: Monad m => Action -> StateT Playfield m Winner
engineS action = do
    moveMissilesS
    runActionS action
    clearOutOfBoundS
    return NoWinner

{-|
Clear out of bounds elements (`Missile`) from the `Playfield` since they can
no longer interact with the rest of the game.
-}
clearOutOfBoundS :: Monad m => StateT Playfield m ()
clearOutOfBoundS = do
    bounds <- use pConstraint
    pMissiles %= filter (isInside bounds . view missilePos)

{-|
Apply an `Action` to the `Playfield`.
-}
runActionS :: Monad m => Action -> StateT Playfield m ()
runActionS DoNothing = return ()
runActionS (Forward tid) = zoom (toTank tid) tankForwardS
runActionS (Backward tid) = zoom (toTank tid) tankBackwardS
runActionS (TurnCat tid a) = zoom (toTank tid) (tankCatTurnS a)
runActionS (TurnTur tid a) = zoom (toTank tid) (tankTurTurnS a)
runActionS (Fire tid) = do
    mMissile <- zoom (toTank tid) tankFireS
    pMissiles %= maybeInsert mMissile
runActionS (DropMine tid) = do
    mMine <- zoom (toTank tid) tankDropMineS
    pMines %= maybeInsert mMine

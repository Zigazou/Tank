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
, playActionS
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
            deriving (Eq, Show)

{-|
Play couples of `Action` on a `Playfield` and look for a `Winner` (or no winner
if the list is exhausted).
-}
playActionS :: Monad m => [(Action, Action)] -> StateT Playfield m Winner
playActionS [] = return NoWinner
playActionS ((action1, action2):actions) = do
    winner <- playS action1 >> playS action2
    if winner == NoWinner then playActionS actions
                          else return winner

{-|
The game itself.
-}
playS :: Monad m => Action -> StateT Playfield m Winner
playS action = do
    moveMissilesS
    runActionS action
    clearOutOfBoundS
    findWinner

{-|
Find which `Tank` is the `Winner`.
-}
findWinner :: Monad m => StateT Playfield m Winner
findWinner = do
    (t1, t2) <- use pTanks
    missiles <- use pMissiles
    let findHit tank = missiles^..folded.missilePos.filtered (cover tank)

    return $ case (findHit t1, findHit t2) of
         ([], []) -> NoWinner
         ([], _) -> Winner TankA
         (_, []) -> Winner TankB
         _ -> DrawGame

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

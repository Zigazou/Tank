{- |
Module      : Tank.Defaults
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Define defaults for the game.
-}
module Tank.Defaults where

import Data.Text as T

import Tank.Units
import Tank.Elements
import Tank.Game

{-|
`Playfield` width
-}
playfieldWidth :: Double
playfieldWidth = 1000

{-|
`Playfield` height
-}
playfieldHeight :: Double
playfieldHeight = 1000

{-|
Generates a `Playfield` given 2 `Tank`s.
-}
makePlayfield :: Tank -> Tank -> Playfield
makePlayfield tankA tankB =
    Playfield (XY 0 0, XY 1000 1000) (tankA, tankB) [] []

{-|
Create a `Tank` based on its coordinates, the direction at which it points to
and its colors. The other values are set by default.
-}
makeTank :: Coords -> Angle -> (T.Text, T.Text) -> Tank
makeTank position direction =
    Tank position
         direction
         direction
         (Reservoir 1000 1000 Fuel)
         (Reservoir 20 20 missile)
         (Reservoir 5 5 mine)
         1.0
    where missile = Missile (XY 0 0) (makeDeg 0) 5.0
          mine = Mine (XY 0 0)

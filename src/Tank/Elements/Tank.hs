{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Tank
Description : Armoured vehicle
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A `Tank` is a vehicle with limited `Mine`, `Missile` and `Fuel`.
-}
module Tank.Elements.Tank
( Tank (..)
  -- * Tank accessors
, tankPos
, tankCatDir
, tankTurDir
, tankFuel
, tankMissile
, tankMine
, tankSpeed
, tankColors
  -- * Tank operations
, cover
, tankForwardS
, tankBackwardS
, tankCatTurnS
, tankTurTurnS
, tankFireS
, tankDropMineS
) where

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State.Lazy

import Tank.Units
import Tank.SVG
import Tank.Elements.Mine
import Tank.Elements.Missile
import Tank.Elements.Fuel

{-|
Describes a tank.
-}
data Tank = Tank
    { _tankPos :: Coords -- ^ Where the `Tank` is
    , _tankCatDir :: Angle -- ^ Where the `Tank` is heading
    , _tankTurDir :: Angle -- ^ Where the `Turret` points to (absolute)
    , _tankFuel :: Reservoir Fuel -- ^ Energy to move the `Tank`
    , _tankMissile :: Reservoir Missile -- ^ `Missile`s to shoot
    , _tankMine :: Reservoir Mine -- ^ `Mine`s to drop
    , _tankSpeed :: Double -- ^ `Tank` speed in units per move
    , _tankColors :: (T.Text, T.Text) -- ^ `Tank` colors (base, turret)
    } deriving (Show, Read)

makeLenses ''Tank

{-|
Tell if a `Tank` covers a `Coord`inate.
-}
cover :: Tank -> Coords -> Bool
cover t c = distance (t^.tankPos) c <= 8

{-|
Moves a `Tank` forward.
-}
tankForwardS :: Monad m => StateT Tank m ()
tankForwardS = do
    speed <- use tankSpeed
    dir <- use tankCatDir
    tankPos += PL speed dir

{-|
Moves a `Tank` backward. Moving a `Tank` backward is twice slower than
forward.
-}
tankBackwardS :: Monad m => StateT Tank m ()
tankBackwardS = do
    speed <- use tankSpeed
    dir <- use tankCatDir
    tankPos -= PL (speed / 2) dir

{-|
Turn the caterpillar (relative). When the caterpillar turns, the turret
follows it.
-}
tankCatTurnS :: Monad m => Angle -> StateT Tank m ()
tankCatTurnS angle = do
    tankCatDir += angle
    tankTurDir += angle

{-|
Turn the turret (relative).
-}
tankTurTurnS :: Monad m => Angle -> StateT Tank m ()
tankTurTurnS = (tankTurDir +=)

{-|
Fire a `Missile` if there is enough remaining.
-}
tankFireS :: Monad m => StateT Tank m (Maybe Missile)
tankFireS = do
    mMissile <- zoom tankMissile rTakeout
    pos <- use tankPos
    angle <- use tankTurDir
    return $ mMissile & _Just.missilePos   .~ pos + PL 30 angle
                      & _Just.missileAngle .~ angle

{-|
Drop a `Mine` if there is enough remaining.
-}
tankDropMineS :: Monad m => StateT Tank m (Maybe Mine)
tankDropMineS = do
    mMine <- zoom tankMine rTakeout
    pos <- use tankPos
    angle <- use tankTurDir
    return $ mMine & _Just.minePos .~ pos + PL 30 angle

instance SVGShow Tank where
    svgCenter _ = XY 17.5 17.5
    svgShow t =
        g [ "transform" =. [position, catRot] ]
            [ g [ "fill" =. ("#888"::T.Text) ]
                [ rect (XY 2.12 1.32) (XY 30.747 4.2932) ""
                , rect (XY 2.12 29.38) (XY 30.747 4.2932) ""
                ]
            , circle (svgCenter t) 17.5 baseColor
            , g [ "transform" =. turRot ]
                [ circle (svgCenter t) 7.0197 turColor
                , rect (XY 18.48 15.35) (XY 14.9 4.2931) "#000"
                ]
            ]
        where (baseColor, turColor) = t^.tankColors
              position = TTranslate (t^.tankPos - svgCenter t)
              catRot = TRotate (t^.tankCatDir) (svgCenter t)
              turRot = TRotate (t^.tankTurDir - t^.tankCatDir) (svgCenter t)

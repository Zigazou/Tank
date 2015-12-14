{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Missile
Description : Something launched to destroy something
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A `Missile` goes at a specific speed in a direction after it was fired.
-}
module Tank.Elements.Missile
( Missile (..)
  -- * Missile lenses
, missilePos
, missileAngle
, missileSpeed
  -- * Missile operations
, moveMissileS
) where

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State.Lazy

import Tank.Units
import Tank.SVG

{-|
A `Missile` is an ammunition which can be fired in a direction at a constant
speed.
-}
data Missile = Missile
    { _missilePos :: Coords -- ^ Position of the missile
    , _missileAngle :: Angle -- ^ Direction where the missile is heading
    , _missileSpeed :: Double -- ^ Speed of the missile
    } deriving (Eq, Show, Read)

makeLenses ''Missile

{-|
Move a `Missile` according to its speed.
-}
moveMissileS :: Monad m => StateT Missile m ()
moveMissileS = do
    speed <- use missileSpeed
    angle <- use missileAngle
    missilePos += PL speed angle

instance SVGShow Missile where
    svgCenter _ = XY 5.977 1.6221
    svgShow m =
        g [ "fill" =. ("#0c3f1b"::T.Text), "transform" =. [position, rotation]]
          [ path "m0 3.2442v-3.2442h8.8056c4.2207 0 4.176 3.2442 0 3.2442z" ""
          ]
        where position = TTranslate (m^.missilePos - svgCenter m)
              rotation = TRotate (m^.missileAngle) (svgCenter m)

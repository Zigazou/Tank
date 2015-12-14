{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Tank.Playfield
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Manage a playfield where the game actually takes place.
-}
module Tank.Game.Playfield
( TankID (TankA, TankB)
, Playfield (Playfield)
, pConstraint
, pTanks
, pMissiles
, pMines
, toTank
, maybeInsert
, moveMissilesS
) where

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State.Lazy

import Tank.Units
import Tank.Elements
import Tank.SVG

{-|
There can only be two `Tank` on a game. They are identified by `TankA` or
`TankB`.
-}
data TankID = TankA | TankB

{-|
A `Playfield` is a rectangular area where two `Tank`s fight. It keeps track of
the `Tank`s positions and of the fired missiles and dropped mines.

It is the state of the game.
-}
data Playfield = Playfield
    { _pConstraint :: (Coords, Coords) -- ^ Limits of the playfield
    , _pTanks :: (Tank, Tank) -- ^ the two players
    , _pMissiles :: [Missile] -- ^ Missiles that have been fired
    , _pMines :: [Mine] -- ^ Mines that have been dropped
    } deriving (Show)

makeLenses ''Playfield

{-|
An accessor to get a specific `Tank` given its identifier (`TankA` or `TankB`).
It avoids having to duplicate code in the `Engine`.

It is meant to be used with `Lens` functions.
-}
toTank :: Functor f => TankID -> (Tank -> f Tank) -> Playfield -> f Playfield
toTank TankA = pTanks._1
toTank TankB = pTanks._2

{-|
Insert an element at the front of a list if it is not `Nothing`.

It is meant to be used with `Lens` functions.
-}
maybeInsert :: Maybe a -> [a] -> [a]
maybeInsert Nothing = id
maybeInsert (Just x) = cons x

{-|
Generates the SVG ViewBox for the rectangular area of a `Playfield`.
-}
toViewBox :: (Coords, Coords) -> T.Text
toViewBox (c1, c2) = T.concat [toAttr c1, " ", toAttr c2]

{-|
Move all the `Missile` on a `Playfield` according to their speed and direction.
-}
moveMissilesS :: Monad m => StateT Playfield m ()
moveMissilesS = zoom (pMissiles.each) moveMissileS

instance SVGShow Playfield where
    svgCenter pf = uncurry center (pf^.pConstraint)

    svgShow pf = svg attrs elems
        where attrs = [ "xmlns" =. ("http://www.w3.org/2000/svg" :: T.Text)
                      , "version" =. ("1.1" :: T.Text)
                      , "viewBox" =. toViewBox (pf^.pConstraint)
                      ]
              elems = [ g [ "transform" =. [ TTranslate (XY 0 1000)
                                           , TScale 1 (-1)
                                           ]
                          ]
                          ([ svgShow $ pf^.pTanks._1
                           , svgShow $ pf^.pTanks._2
                           ] ++ (svgShow <$> pf^.pMissiles))
                      ]

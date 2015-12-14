{- |
Module      : Main
Description : The Tank game
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The Tank game is a simple game where 2 tanks fight each other on a playfield.
They can fire missiles, drop mines in order to destroy the opponent.
-}
module Main where

import qualified Data.ByteString.Char8 as BIO
import Text.XML.Generator

import Tank.Units
import Tank.Game
import Tank.SVG
import Tank.Defaults

{-|
Initialize the game and run some actions on it.
-}
main :: IO ()
main = do
    let tankA = makeTank (XY 500.0 30.0)
                         (makeDeg 90)
                         ("#c5c1ec", "#7262e1")

        tankB = makeTank (XY 500.0 970.0)
                         (makeDeg (-90)) 
                         ("#c1ecce", "#62e186")

        playfield = makePlayfield tankA tankB

    let actions = [ Forward TankA
                  , Forward TankB
                  , TurnTur TankA 90
                  , TurnTur TankB 90
                  , Fire TankA
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , TurnTur TankA (-30)
                  , Fire TankA
                  , TurnTur TankB (-30)
                  , Fire TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  , Forward TankA
                  , Forward TankB
                  ]

    let playfield' = foldl engine playfield actions

    let pfsvg = makeSvg (svgShow playfield') 
    BIO.putStrLn $ xrender pfsvg
    --print playfield'
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
import Control.Monad.State.Lazy (runState)
import System.IO (hPrint, stderr)

import Tank.Units (Coords (XY), makeDeg)
import Tank.Game ( Action (Forward, TurnTur, Fire, DoNothing)
                 , TankID (TankA, TankB)
                 , playActionS
                 )
import Tank.SVG (svgShow, svgRender, makeSvg)
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

        actionsA = [ Forward TankA
                   , TurnTur TankA 90
                   , Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , TurnTur TankA (-30), Fire TankA
                   , Forward TankA
                   , Forward TankA
                   , Forward TankA
                   , Forward TankA
                   , Forward TankA
                   , Forward TankA
                   , Forward TankA
                   ] ++ cycle [DoNothing]

        actionsB = [ Forward TankB
                   , TurnTur TankB 90
                   , Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , TurnTur TankB (-30), Fire TankB
                   , Forward TankB
                   , Forward TankB
                   , Forward TankB
                   , Forward TankB
                   , Forward TankB
                   , Forward TankB
                   , Forward TankB
                   ] ++ cycle [DoNothing]

    let actions = zip actionsA actionsB
        (winner, playfield') = runState (playActionS actions) playfield

    let pfsvg = makeSvg (svgShow playfield') 
    BIO.putStrLn $ svgRender pfsvg
    hPrint stderr winner
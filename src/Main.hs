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
import System.IO (hPutStrLn, stderr)
import ParseActions (parseActions)

import Tank.Units (Coords (XY), makeDeg)
import Tank.Game (TankID (TankA, TankB), playActionS)
import Tank.SVG (svgShow, svgRender, makeSvg)
import Tank.Defaults

{-|
Initialize the game and run some actions on it.
-}
main :: IO ()
main = do
    actionsATxt <- BIO.readFile "test/actionsA.txt"
    actionsBTxt <- BIO.readFile "test/actionsB.txt"

    let tankA = makeTank (XY 500.0 30.0)
                         (makeDeg 90)
                         ("#c5c1ec", "#7262e1")

        tankB = makeTank (XY 500.0 970.0)
                         (makeDeg (-90)) 
                         ("#c1ecce", "#62e186")

        playfield = makePlayfield tankA tankB

        actionsA = parseActions TankA actionsATxt
        actionsB = parseActions TankB actionsBTxt

    case (actionsA, actionsB) of
         (Left msg, _) -> hPutStrLn stderr msg
         (_, Left msg) -> hPutStrLn stderr msg
         (Right a, Right b) -> do
            let actions = zip a b
                (winner, playfield') = runState (playActionS actions) playfield
                pfsvg = makeSvg (svgShow playfield') 
            BIO.putStrLn $ svgRender pfsvg
            hPutStrLn stderr $ show winner

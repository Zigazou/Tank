{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : TestParserActions
Description : Tests the parser of actions
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Data.Either (isLeft)

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Text.IO as TIO

import Tank.Game (TankID (TankA))
import ParseActions (parseActions)

-- Test valid files
validFiles :: [ValidFile]
validFiles =
    [ ValidFile "test/valid/oneAction0.txt" 1
    , ValidFile "test/valid/oneAction1.txt" 1
    , ValidFile "test/valid/oneAction2.txt" 1
    , ValidFile "test/valid/someActions0.txt" 7
    ]

data ValidFile = ValidFile String Int deriving Show
instance Arbitrary ValidFile where
    arbitrary = elements validFiles

prop_validProgram :: ValidFile -> Property
prop_validProgram (ValidFile fp count) = monadicIO $ do
    source <- run $ TIO.readFile fp
    case parseActions TankA source of
         Left msg -> fail $ show msg
         Right actions ->
            case count == length actions of
                 True -> return ()
                 False -> fail $ "Invalid number of actions " ++ show (length actions)

-- Tests invalid grids
invalidFiles :: [InvalidFile]
invalidFiles = [ InvalidFile "test/invalid/noAction0.txt"
               , InvalidFile "test/invalid/noAction1.txt"
               , InvalidFile "test/invalid/noAction2.txt"
               , InvalidFile "test/invalid/unknownAction0.txt"
               , InvalidFile "test/invalid/unknownAction1.txt"
               , InvalidFile "test/invalid/unknownAction2.txt"
               , InvalidFile "test/invalid/unknownAction3.txt"
               , InvalidFile "test/invalid/wrongAngle0.txt"
               ]

newtype InvalidFile = InvalidFile String deriving Show
instance Arbitrary InvalidFile where
    arbitrary = elements invalidFiles

prop_invalidProgram :: InvalidFile -> Property
prop_invalidProgram (InvalidFile fp) = monadicIO $ do
    source <- run $ TIO.readFile fp
    assert $ isLeft $ parseActions TankA source

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure

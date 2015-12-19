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
    [ ValidFile "test/valid/noAction0.txt" 0
    , ValidFile "test/valid/noAction1.txt" 0
    , ValidFile "test/valid/noAction2.txt" 0
    , ValidFile "test/valid/oneAction0.txt" 1
    , ValidFile "test/valid/oneAction1.txt" 1
    , ValidFile "test/valid/oneAction2.txt" 1
    ]

data ValidFile = ValidFile String Int deriving Show
instance Arbitrary ValidFile where
    arbitrary = elements validFiles

prop_validGrid :: ValidFile -> Property
prop_validGrid (ValidFile fp count) = monadicIO $ do
    source <- run $ TIO.readFile fp
    case parseActions TankA source of
         Left msg -> fail $ show msg
         Right actions -> assert $ count == length actions

-- Tests invalid grids
invalidFiles :: [InvalidFile]
invalidFiles = [ InvalidFile "test/invalid/unknownAction0.txt"
               , InvalidFile "test/invalid/unknownAction1.txt"
               ]

newtype InvalidFile = InvalidFile String deriving Show
instance Arbitrary InvalidFile where
    arbitrary = elements invalidFiles

prop_invalid :: InvalidFile -> Property
prop_invalid (InvalidFile fp) = monadicIO $ do
    source <- run $ TIO.readFile fp
    assert $ isLeft $ parseActions TankA source

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure

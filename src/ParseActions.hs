{- |
Module      : ParseActions
Description : Parse Actions to be applied to a Tank
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Handle `Action` parsing and error messages.
-}
module ParseActions (parseActions) where

import qualified Data.ByteString.Char8 as BIO
import Data.Attoparsec.ByteString.Char8 (parse, IResult (Fail, Done, Partial))
import Data.List (intercalate)

import Tank.Game (TankID, actionsParser, Action)

{-|
Parse a `ByteString` into a list of `Action` for a specific `TankID`. 
If it fails to parse, it returns an error message indicating where it failed.
-}
parseActions :: TankID -> BIO.ByteString -> Either String [Action]
parseActions tid raw = checkParse $ parse (actionsParser tid) raw
    where
        checkParse :: IResult BIO.ByteString [Action] -> Either String [Action]
        checkParse (Fail remain ctxs _) = Left $ errorMessage raw remain tid ctxs
        checkParse (Partial p) = checkParse (p BIO.empty)
        checkParse (Done _ actions) = Right actions

offsetToRow :: BIO.ByteString -> Int -> Int
offsetToRow b offset = length (BIO.lines (BIO.take offset b))

firstLine :: BIO.ByteString -> BIO.ByteString
firstLine = BIO.takeWhile (/= '\n')

errorMessage :: BIO.ByteString -> BIO.ByteString -> TankID -> [String] -> String
errorMessage input remain tid ctxs = concat [ intercalate " > " ctxs
                                            , " for ", show tid
                                            , " at line: ", show row
                                            , "\n    --> "
                                            , BIO.unpack $ firstLine remain
                                            ]
    where offset = BIO.length input - BIO.length remain
          row = offsetToRow input offset + 1

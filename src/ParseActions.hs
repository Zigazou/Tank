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

import Text.ParserCombinators.Parsec
       ( GenParser, ParseError, runParser, oneOf, skipMany1, (<?>), try, many1
       , getState
       )
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.Prim (parserFail)
import Text.Parsec.Char (endOfLine)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative (many, (<|>))
import Control.Monad (liftM)

import Tank.Units (makeDeg)
import Tank.Game (TankID, Action (..))

{-|
Parse a `ByteString` into a list of `Action` for a specific `TankID`. 
If it fails to parse, it returns an error message indicating where it failed.
-}
parseActions :: TankID -> Text -> Either ParseError [Action]
parseActions tid raw = runParser program tid (show tid) (T.unpack raw)

{-|
A white space is either a space or a tabulation.
-}
whitespace :: GenParser Char st Char
whitespace = oneOf " \t"

{-|
White spaces are a list of white space with 0 or more elements.
-}
whitespaces :: GenParser Char st String
whitespaces = many whitespace

{-|
A program is made of lines containing or not actions.
-}
program :: GenParser Char TankID [Action]
program = many (actionLine <* many emptyLine) <?> "program"

{-|
An empty line is a line which contains nothing else than white spaces.
-}
emptyLine :: GenParser Char st Char
emptyLine = whitespaces >> endOfLine

{-|
An action line is a command surrounded or not by white spaces.
-}
actionLine :: GenParser Char TankID Action
actionLine = do
    _ <- whitespaces
    c <- command
    _ <- whitespaces
    _ <- endOfLine
    return c

{-|
A command is either a simple action or an angle action.
-}
command :: GenParser Char TankID Action
command = try simpleAction <|> angleAction <?> "command"

{-|
Given a list of allowed names, parse an action name and returns its constructor.
-}
actionByName :: [(String, f)] -> GenParser Char st f
actionByName names = do
    name <- many1 (oneOf ['a'..'z']) <?> "action"
    maybe (pfail name) return (lookup name names)
    where pfail x = parserFail $ "unknown action '" ++ x ++ "'"

{-|
Parse a simple action. A simple action is an action which does not require any
parameter.
-}
simpleAction :: GenParser Char TankID Action
simpleAction = do
    action <- actionByName simpleActions
    liftM action getState

    where simpleActions = [ ("forward", Forward)
                          , ("backward", Backward)
                          , ("fire", Fire)
                          , ("dropmine", DropMine)
                          ]

{-|
Parse an angle action. An angle action is an action which requires an angle
as a parameter. The angle is an integer and is mesured in degrees.
-}
angleAction :: GenParser Char TankID Action
angleAction = do
    action <- actionByName angleActions
    _ <- skipMany1 whitespace
    d <- int
    let angle = makeDeg $ fromIntegral (d :: Integer)
    liftM (flip action angle) getState

    where angleActions = [ ("turncaterpillar", TurnCat)
                         , ("turnturret", TurnTur)
                         ]

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

import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Prim (parserFail)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative

import Tank.Units (Angle, makeDeg)
import Tank.Game (TankID, Action (..))

{-|
Parse a `ByteString` into a list of `Action` for a specific `TankID`. 
If it fails to parse, it returns an error message indicating where it failed.
-}
parseActions :: TankID -> Text -> Either ParseError [Action]
parseActions tid raw = parse (program tid) (show tid) (T.unpack raw)

sep :: GenParser Char st Char
sep = char ' ' <|> char '\n' <|> char '\r' <?> "separator"

signedDecimal :: GenParser Char st Integer
signedDecimal = do
    sign <- optional (char '-')
    digits <- many1 digit
    return . read $ case sign of
        Nothing -> digits
        Just _ -> '-':digits

simpleActions :: [(String, TankID -> Action)]
simpleActions = [ ("forward", Forward)
                , ("backward", Backward)
                , ("fire", Fire)
                , ("dropmine", DropMine)
                ]

angleActions :: [(String, TankID -> Angle -> Action)]
angleActions = [ ("turncaterpillar", TurnCat)
               , ("turnturret", TurnTur)
               ]

program :: TankID -> GenParser Char st [Action]
program tid = sepBy (command tid) (many1 sep)
            <* many sep <* eof
            <?> "program"

command :: TankID -> GenParser Char st Action
command tid = try (simpleAction tid)
              <|> (angleAction tid)
              <?> "command"

actionName :: GenParser Char st String
actionName = many1 (satisfy isAlpha) <?> "action"

actionByName :: [(String, f)] -> GenParser Char st f
actionByName names = do
    name <- actionName
    case lookup name names of
        Just l -> return l
        Nothing -> parserFail $ "unknown action '" ++ name ++ "'"

simpleAction :: TankID -> GenParser Char st Action
simpleAction tid = do
    action <- actionByName simpleActions
    return $ action tid

angleAction :: TankID -> GenParser Char st Action
angleAction tid = do
    action <- actionByName angleActions
    _ <- skipMany1 sep
    d <- signedDecimal
    let angle = makeDeg (fromIntegral d)
    return $ action tid angle
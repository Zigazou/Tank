{- |
Module      : Action
Description : Available actions to a player
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Game.Action
( Action (Forward, Backward, TurnCat, TurnTur, Fire, DropMine, DoNothing)
, actionsParser
, actionParser
) where

import Data.Attoparsec.ByteString.Char8
import Data.ByteString
import Control.Applicative

import Tank.Units
import Tank.Game.Playfield

{-|
Authorized `Action` are among the following.
-}
data Action = Forward TankID -- ^ Make the `Tank` go forward
            | Backward TankID -- ^ Make the `Tank` go backward
            | TurnCat TankID Angle -- ^ Turn the caterpillar
            | TurnTur TankID Angle -- ^ Turn the turret
            | Fire TankID -- ^ Fire a `Missile`
            | DropMine TankID -- ^ Drop a `Mine` where the `Tank` stands
            | DoNothing -- ^ Do nothing
            deriving (Eq, Show)

actionsParser :: TankID -> Parser [Action]
actionsParser tid = manyTill (actionParser tid <* endOfLine) endOfInput

actionParser :: TankID -> Parser Action
actionParser tid =
     (stringCI "forward"            >> return (Forward tid))
 <|> (stringCI "backward"           >> return (Backward tid))
 <|> (actionAngle "turncaterpillar" >>= return . TurnCat tid)
 <|> (actionAngle "turnturret"      >>= return . TurnTur tid)
 <|> (stringCI "fire"               >> return (Fire tid))
 <|> (stringCI "dropmine"           >> return (DropMine tid))
 <|> (stringCI "donothing"          >> return DoNothing)
 <?> "unknown action"

actionAngle :: ByteString -> Parser Angle
actionAngle actionTag = do
    _ <- stringCI actionTag
    _ <- skipSpace
    d <- signed decimal
    return $ makeDeg (fromIntegral (d :: Integer))

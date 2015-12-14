{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Reservoir
Description : Manage coordinates in either polar or cartesian system
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Units.Reservoir
( Reservoir (Reservoir)
, rQuantity
, rMaximum
, rContent
, rTakeout
) where

import Control.Lens
import Control.Monad.State.Lazy

{-|
A `Reservoir` can contain something in a limited quantity.
-}
data Reservoir a = Reservoir
    { _rQuantity :: Int -- ^ Current quantity
    , _rMaximum :: Int -- ^ Maximum capacity
    , _rContent :: a -- ^ What the `Reservoir` contains
    } deriving (Show, Read)

makeLenses ''Reservoir

{-|
State transformer that takes out one unit from a `Reservoir`.
-}
rTakeout :: Monad m => StateT (Reservoir a) m (Maybe a)
rTakeout = do
    qty <- use rQuantity
    if qty == 0 then return Nothing
                else liftM Just (rQuantity -= 1 >> use rContent)

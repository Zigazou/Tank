{- |
Module      : Angle
Description : Manage normalized angles in degrees or radians
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Units.Angle
( Angle (Deg, Rad)
  -- * Conversion
, makeDeg
, makeRad
, toDeg
, toRad
  -- * Trigonometric functions
, sine
, cosine
, arctan
, arccosine
, arcsine
) where

{-|
An `Angle` can be expressed either in degrees or in radians. The functions
manipulating this type will generate only normalized angles:

- degrees will be constrained in [0°, 360°[
- radians will be constrained in ]-pi, pi]

Positive angles works anti-clockwise.

`Angle` should be created using `makeDeg` or `makeRad` functions, not using
the constructors.
-}
data Angle = Deg Double
           | Rad Double
           deriving (Show, Read)

{-|
Generate a normalized `Angle` in degrees. Values are constrained in
[0°, 360°].
-}
makeDeg :: Double -> Angle
makeDeg v = Deg v'
    where frac x = x - fromInteger (round x)
          ratio = frac (abs v / 360)
          v' | v == 360 = 0
             | v < 0 = 360 - 360 * ratio
             | v > 360 = 360 * ratio
             | otherwise = v

{-|
Generate a normalized `Angle` in radians. Values are constrained in
[0°, 360°].
-}
makeRad :: Double -> Angle
makeRad v = Rad $ v - 2 * pi * (fromInteger . floor) ((v + pi) / (2 * pi))

{-|
Convert a degree `Angle` to a radian `Angle`.
-}
toRad :: Angle -> Angle
toRad (Deg d) = makeRad (0.01745329252 * d)
toRad a = a

{-|
Convert a radian `Angle` to a degree `Angle`.
-}
toDeg :: Angle -> Angle
toDeg (Rad r) = makeDeg (57.295779513 * r)
toDeg a = a

instance Eq Angle where
    (==) (Deg d1) (Deg d2) = d1 == d2
    (==) (Rad r1) (Rad r2) = r1 == r2
    (==) a1@(Deg _) a2 = a1 == toDeg a2
    (==) a1@(Rad _) a2 = a1 == toRad a2

instance Ord Angle where
    compare (Deg d1) (Deg d2) = compare d1 d2
    compare (Rad r1) (Rad r2) = compare r1 r2
    compare a1@(Deg _) a2 = compare a1 (toDeg a2)
    compare a1@(Rad _) a2 = compare a1 (toRad a2)

{-|
Helper functions applying an operator to two `Angle`, keeping the unit of
the first operand in the result (ie Deg x + Rad y = Deg z)
-}
angleOperation :: (Double -> Double -> Double) -> Angle -> Angle -> Angle
angleOperation f (Deg d1) (Deg d2) = makeDeg (f d1 d2)
angleOperation f a1@(Deg _) a2 = angleOperation f a1 (toDeg a2)
angleOperation f (Rad d1) (Rad d2) = makeRad (f d1 d2)
angleOperation f a1@(Rad _) a2 = angleOperation f a1 (toRad a2)

instance Num Angle where
    (+) = angleOperation (+)
    (-) = angleOperation (-)
    (*) = angleOperation (*)
    negate (Deg d) = makeDeg (0.0 - d)
    negate (Rad r) = makeRad (0.0 - r)
    abs = id
    signum _ = Deg 1.0
    fromInteger = makeDeg . fromInteger

{-|
Sine function working on `Angle` type.
-}
sine :: Angle -> Double
sine (Rad r) = sin r
sine a = sine (toRad a)

{-|
Cosine function working on `Angle` type.
-}
cosine :: Angle -> Double
cosine (Rad r) = cos r
cosine a = cosine (toRad a)

{-|
Arctangent function generating an `Angle` value in radians.
-}
arctan :: Double -> Double -> Angle
arctan y = makeRad . atan2 y

{-|
Arccosine function generating an `Angle` value in radians
-}
arccosine :: Double -> Angle
arccosine = makeRad . acos

{-|
Arcsine function generating an `Angle` value in radians
-}
arcsine :: Double -> Angle
arcsine = makeRad . asin

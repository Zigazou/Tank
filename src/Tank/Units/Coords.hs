{- |
Module      : Coords
Description : Manage coordinates in either polar or cartesian system
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.Units.Coords
( Coords (XY, PL)
, toXY
, toPL
, toTuple
, center
, isInside
) where

import Tank.Units.Angle

{-|
Coordinates can be either expressed as cartesian coordinates or polar
coordinates.
-}
data Coords = XY Double Double -- ^ Cartesian coordinates
            | PL Double Angle -- ^ Polar coordinates
            deriving (Show, Read)

{-|
Converts `PL` coordinates to `XY` coordinates.
-}
toXY :: Coords -> Coords
toXY c@(XY _ _) = c
toXY (PL dist angle) = XY (dist * cosine angle) (dist * sine angle)

{-|
Converts `XY` coordinates to `PL` coordinates.
-}
toPL :: Coords -> Coords
toPL c@(PL _ _) = c
toPL (XY x y) = PL (sqrt $ x * x + y * y) (arctan y x)

{-|
Converts coordinates to cartesian coordinates in a tuple of Double.
-}
toTuple :: Coords -> (Double, Double)
toTuple (XY x y) = (x, y)
toTuple c = (toTuple . toXY) c

{-|
Calculate center of two points.
-}
center :: Coords -> Coords -> Coords
center (XY x1 y1) (XY x2 y2) = XY ((x1 + x2) / 2) ((y1 + y2) / 2)
center c1@(XY _ _) c2 = center c1 (toXY c2)
center c1 c2 = toPL $ center (toXY c1) (toXY c2)

{-|
Tells whether a `Coords` is inside the rectangle formed by 2 `Coords`. Returns
`True` if this is the case, `False` otherwise.
-}
isInside :: (Coords, Coords) -> Coords -> Bool
isInside ((XY x1 y1), (XY x2 y2)) (XY x y) =
    (x >= x1 && x <= x2) && (y >= y1 && y <= y2)
isInside (mini, maxi) c = isInside ((toXY mini, toXY maxi)) (toXY c)

instance Eq Coords where
    (==) (XY x1 y1) (XY x2 y2) = x1 == x2 && y1 == y2
    (==) (PL d1 a1) (PL d2 a2) = d1 == d2 && a1 == a2
    (==) c1@(XY _ _) c2 = c1 == toXY c2
    (==) c1@(PL _ _) c2 = c1 == toPL c2

instance Num Coords where
    (+) (XY x1 y1) (XY x2 y2) = XY (x1 + x2) (y1 + y2)
    (+) (PL d1 a1) (PL d2 a2) = PL d a
        where ca2a1 = cosine (a2 - a1)
              d = sqrt (d1 * d1 + d2 * d2 + 2 * d1 * d2 * ca2a1)
              a = a1 + arccosine ( (d1 + d2 * ca2a1) / d)
    (+) c1@(XY _ _) c2 = c1 + toXY c2
    (+) c1@(PL _ _) c2 = c1 + toPL c2

    (*) (XY x1 y1) (XY x2 y2) = XY (x1 * x2 - y1 * y2) (x1 * y2 + y1 * x2)
    (*) c1@(PL _ _) c2@(PL _ _) = toPL (toXY c1 * toXY c2)
    (*) c1@(XY _ _) c2 = c1 * toXY c2
    (*) c1@(PL _ _) c2 = c1 * toPL c2

    negate (XY x y) = XY (-x) (-y)
    negate (PL d a) = PL d (-a)

    signum (XY x y) = XY (signum x) (signum y)
    signum c =  (toPL . signum . toXY) c

    abs (XY x y) = XY (abs x) (abs y)
    abs (PL d a) = PL d (abs a)

    fromInteger n = XY (fromInteger n) 0.0

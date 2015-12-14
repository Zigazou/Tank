{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Transform
Description : SVG transformations
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.SVG.Transform
( Transform (TMatrix, TScale, TTranslate, TRotate ,TSkewX, TSkewY)
, makeText
) where

import qualified Data.Text as T

import Tank.Units

import Tank.SVG.Attribute

{-|
All the transformation SVG allows.
-}
data Transform = TMatrix Double Double Double Double Double Double
               | TScale Double Double
               | TTranslate Coords
               | TRotate Angle Coords
               | TSkewX Angle
               | TSkewY Angle

instance SVGAttribute Transform where
    toAttr (TMatrix a b c d e f) = makeText "matrix" [a, b, c, d, e, f]
    toAttr (TScale x y) = makeText "scale" [x, y]
    toAttr (TTranslate c) = makeText "translate" [c]
    toAttr (TRotate a c) = makeText "rotate" [toAttr a, toAttr c]
    toAttr (TSkewX a) = makeText "skewX" [a]
    toAttr (TSkewY a) = makeText "skewY" [a]

instance SVGAttribute [Transform] where
    toAttr transforms = T.intercalate " " $ toAttr <$> transforms

{-|
Generate the textual representation of a transformation.
-}
makeText :: SVGAttribute a => T.Text -> [a] -> T.Text
makeText name values = T.concat [ name, "(", tvalues, ")" ]
    where tvalues = T.intercalate " " $ toAttr <$> values

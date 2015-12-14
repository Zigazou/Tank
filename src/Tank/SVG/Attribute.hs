{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Attribute
Description : SVG attribute
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.SVG.Attribute (SVGAttribute (toAttr), (=.)) where

import qualified Data.Text as T
import Text.XML.Generator
import Tank.Units

{-|
Types obeying the `SVGAttribute` class are able to return a `Text` value.
-}
class SVGAttribute a where
    toAttr :: a -> T.Text

instance SVGAttribute [Char] where
    toAttr = T.pack

instance SVGAttribute Double where
    toAttr = T.pack . show 

instance SVGAttribute Coords where
    toAttr c = T.concat [toAttr x, " ", toAttr y]
        where (x, y) = toTuple c

instance SVGAttribute Angle where
    toAttr (Deg d) = (T.pack . show) d
    toAttr a = (toAttr . toDeg) a

instance SVGAttribute T.Text where
    toAttr = id

{-|
Given a name and a value, it generates an `Xml` `Attr`. The name must be a
`Text` but the value can be any type instanciating the `SVGAttribute` class.
-}
(=.) :: SVGAttribute a => T.Text -> a -> Xml Attr
(=.) name value = xattr name (toAttr value)

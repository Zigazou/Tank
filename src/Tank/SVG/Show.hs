{- |
Module      : Show
Description : SVG show
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Tank.SVG.Show (makeSvg, snode, SVGShow (svgCenter, svgShow), svgRender)
where

import qualified Data.Text as T
import Text.XML.Generator

import Tank.Units

svgDocInfo :: DocInfo
svgDocInfo = defaultDocInfo { docInfo_standalone = False }

{-|
Given `Xml` `Elem`, generates an SVG document in the form of a `Xml` `Doc`.
-}
makeSvg :: Xml Elem -> Xml Doc
makeSvg = doc svgDocInfo

{-|
Render an `Xml` document into raw SVG.
-}
svgRender :: (Renderable r, XmlOutput t) => Xml r -> t
svgRender = xrender

{-|
Generate a node `Xml` `Elem` given its name, its attributes and its children.
-}
snode :: T.Text -> [Xml Attr] -> [Xml Elem] -> Xml Elem
snode name attrs [] = xelem name (xattrs attrs, noElems) 
snode name attrs elems = xelem name (xattrs attrs, xelems elems) 

{-|
Types obeying this class are able to create `Xml` `Elem` to represent their
values in an SVG document.
-}
class SVGShow a where
    {-|
    Returns the center of the representation of the value
    -}
    svgCenter :: a -> Coords

    {-|
    Generate an `Xml` `Elem` representing a value in an SVG document
    -}
    svgShow :: a -> Xml Elem

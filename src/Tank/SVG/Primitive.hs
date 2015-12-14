{- |
Module      : Primitive
Description : SVG primitives
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

SVG primitives like a circle or rect for SVG generation.
-}
module Tank.SVG.Primitive (circle, rect, path, g, svg) where

import qualified Data.Text as T
import Text.XML.Generator

import Tank.SVG.Attribute
import Tank.SVG.Show

import Tank.Units

{-|
Generate an SVG circle.
-}
circle :: Coords -- ^ Center of the circle
       -> Double -- ^ Radius of the circle
       -> T.Text -- ^ Fill-in color in #RRGGBB format
       -> Xml Elem
circle c r fill = snode "circle"
                        [ "cx" =. x
                        , "cy" =. y
                        , "r" =. r
                        , "fill" =. fill
                        ]
                        []
    where (x, y) = toTuple c

{-|
Generate an SVG rectangle.
-}
rect :: Coords -- ^ Top-left corner coordinates
     -> Coords -- ^ Width and height
     -> T.Text -- ^ Fill-in color in #RRGGBB format
     -> Xml Elem
rect c d fill = snode "rect"
                      [ "x" =. x
                      , "y" =. y
                      , "width" =. w
                      , "height" =. h
                      , "fill" =. fill
                      ]
                      []
    where ((x, y), (w, h)) = (toTuple c, toTuple d)

{-|
Generate an SVG path.
-}
path :: T.Text -- ^ A string containing the path
     -> T.Text -- ^ Fill-in color in #RRGGBB format
     -> Xml Elem
path d fill = snode "path" [ "d" =. d, "fill" =. fill ] []

{-|
Group multiple elements and generate an SVG g element.
-}
g :: [Xml Attr] -- ^ List of attributes
  -> [Xml Elem] -- ^ List of children
  -> Xml Elem
g = snode "g"

{-|
Generate the top SVG element
-}
svg :: [Xml Attr] -- ^ List of attributes
    -> [Xml Elem] -- ^ List of children
    -> Xml Elem
svg = snode "svg"
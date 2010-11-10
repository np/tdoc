{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Text.TDoc.Attributes where

import Text.TDoc.Core

data Length = Px Int
            | Cm Int
            | Em Int

instance Show Length where
  show (Px x) = show x ++ "px"
  show (Cm x) = show x ++ "cm"
  show (Em x) = show x ++ "em"

toPixels :: Length -> Int
toPixels (Px x) = x
toPixels _ = error "toPixels: wrong unit"

newtype  Width = Width { fromWidth :: Length }
class    WidthAttrTag t where widthTag :: t Width
instance IsAttribute Width
width    :: (WidthAttrTag t, Width `IsAttributeOf` node) => Length -> AttributeOf t node
width    = TAttr widthTag . Width

newtype  Height = Height { fromHeight :: Length }
instance IsAttribute Height
class    HeightAttrTag t where heightTag :: t Height
height   :: (HeightAttrTag t, Height `IsAttributeOf` node) => Length -> AttributeOf t node
height   = TAttr heightTag . Height

class AttributeTags t where
  altTag        :: t Alt
  nameTag       :: t Name
  sizeTag       :: t Size
  rowsTag       :: t Rows
  colsTag       :: t Cols

instance IsAttribute Alt
instance IsAttribute Name
instance IsAttribute Size
instance IsAttribute Rows
instance IsAttribute Cols

newtype Src = Src { fromSrc :: String }
instance IsAttribute Src
class SrcAttrTag t where
  srcTag :: t Src
src :: (SrcAttrTag t, Src `IsAttributeOf` node) => String -> AttributeOf t node
src = TAttr srcTag . Src

newtype Size = Size { fromSize :: Int }

newtype Alt = Alt { fromAlt :: String }

newtype ClassAttr = ClassAttr { fromClassAttr :: String }
class ClassAttrTag t where classAttrTag  :: t ClassAttr
instance IsAttribute ClassAttr
instance IsNode a => IsAttributeOf ClassAttr a
classAttr :: (ClassAttrTag t, IsNode a) => String -> AttributeOf t a
classAttr = TAttr classAttrTag . ClassAttr

newtype Name = Name { fromName :: String }
newtype Rows = Rows { fromRows :: Int }
newtype Cols = Cols { fromCols :: Int }

newtype  Style = Style { fromStyle :: String } -- put something more typeful
instance IsAttribute Style
instance IsNode n => IsAttributeOf Style n
class    StyleAttrTag t where styleTag :: t Style
style    :: (StyleAttrTag t, Style `IsAttributeOf` a) => String -> AttributeOf t a
style    = TAttr styleTag . Style

alt :: (AttributeTags t, Alt `IsAttributeOf` node) => String -> AttributeOf t node
alt = TAttr altTag . Alt

name :: (AttributeTags t, IsAttributeOf Name a) => String -> AttributeOf t a
name = TAttr nameTag . Name

rows :: (AttributeTags t, IsAttributeOf Rows a) => Int -> AttributeOf t a
rows = TAttr rowsTag . Rows

cols :: (AttributeTags t, IsAttributeOf Cols a) => Int -> AttributeOf t a
cols = TAttr colsTag . Cols

size :: (AttributeTags t, IsAttributeOf Size a) => Int -> AttributeOf t a
size = TAttr sizeTag . Size

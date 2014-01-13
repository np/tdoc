--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.Attributes
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TemplateHaskell, TypeOperators #-}
module Text.TDoc.Attributes where

import Text.TDoc.Core
import Text.TDoc.TH (attributes)

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

newtype Width       = Width  { fromWidth  :: Length }
newtype Height      = Height { fromHeight :: Length }
newtype Src         = Src    { fromSrc    :: String }
newtype Size        = Size   { fromSize   :: Int }
newtype Alt         = Alt    { fromAlt    :: String }
newtype ClassAttr   = ClassAttr { fromClassAttr :: String }
newtype Name        = Name { fromName :: String }
newtype Rows        = Rows { fromRows :: Int }
newtype Cols        = Cols { fromCols :: Int }
newtype Style       = Style { fromStyle :: String } -- put something more typeful
newtype Identifier  = Identifier { fromIdentifier :: String }
newtype Href        = Href { fromHref :: String }
newtype TitleAttr   = TitleAttr { fromTitleAttr :: String }

$(attributes [''Width, ''Height, ''Src, ''Size, ''Alt, ''ClassAttr, ''Name
             ,''Rows, ''Cols, ''Style, ''Identifier, ''Href, ''TitleAttr])

-- Common attributes
instance IsNode a => IsAttributeOf ClassAttr a
instance IsNode n => IsAttributeOf Style n
instance IsNode n => IsAttributeOf Identifier n

width      :: (WidthTag t, Width `IsAttributeOf` node) => Length -> AttributeOf t node
width      = TAttr widthTag . Width

height     :: (HeightTag t, Height `IsAttributeOf` node) => Length -> AttributeOf t node
height     = TAttr heightTag . Height

src        :: (SrcTag t, Src `IsAttributeOf` node) => String -> AttributeOf t node
src        = TAttr srcTag . Src

size       :: (SizeTag t, Size `IsAttributeOf` a) => Int -> AttributeOf t a
size       = TAttr sizeTag . Size

alt        :: (AltTag t, Alt `IsAttributeOf` node) => String -> AttributeOf t node
alt        = TAttr altTag . Alt

classAttr  :: (ClassAttrTag t, IsNode a) => String -> AttributeOf t a
classAttr  = TAttr classAttrTag . ClassAttr

name       :: (NameTag t, Name `IsAttributeOf` a) => String -> AttributeOf t a
name       = TAttr nameTag . Name

rows       :: (RowsTag t, Rows `IsAttributeOf` a) => Int -> AttributeOf t a
rows       = TAttr rowsTag . Rows

cols       :: (ColsTag t, Cols `IsAttributeOf` a) => Int -> AttributeOf t a
cols       = TAttr colsTag . Cols

style      :: (StyleTag t, Style `IsAttributeOf` a) => String -> AttributeOf t a
style      = TAttr styleTag . Style

identifier :: (IdentifierTag t, Identifier `IsAttributeOf` a) => String -> AttributeOf t a
identifier = TAttr identifierTag . Identifier

href       :: (HrefTag t, Href `IsAttributeOf` a) => String -> AttributeOf t a
href       = TAttr hrefTag . Href

titleAttr  :: (TitleAttrTag t, TitleAttr `IsAttributeOf` a) => String -> AttributeOf t a
titleAttr  = TAttr titleAttrTag . TitleAttr

class (WidthTag t
      ,HeightTag t
      ,SrcTag t
      ,ClassAttrTag t
      ,StyleTag t
      ,SizeTag t
      ,AltTag t
      ,NameTag t
      ,RowsTag t
      ,ColsTag t
      ,IdentifierTag t
      ,HrefTag t
      ,TitleAttrTag t
      ) => AttributeTags t

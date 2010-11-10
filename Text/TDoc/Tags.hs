{-# LANGUAGE TypeFamilies, ScopedTypeVariables, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Text.TDoc.Tags where

import Text.TDoc.Core
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy

data Root
data Preambule
data Document
data Subsection
data Section
data UList
data Item
data Paragraph
data Span
data HLink
data Title
data Image
data Br
data Hr
data Table
data Row
data Col
data HCol
data Div a
data Leaf
newtype Size = Size { fromSize :: Int }
newtype Style = Style { fromStyle :: String } -- put something more typeful
newtype Url = Url { fromUrl :: String }
newtype Alt = Alt { fromAlt :: String }
newtype Src = Src { fromSrc :: String }
newtype ClassAttr = ClassAttr { fromClassAttr :: String }
newtype Name = Name { fromName :: String }
newtype Rows = Rows { fromRows :: Int }
newtype Cols = Cols { fromCols :: Int }

data Length = Px Int
            | Cm Int
            | Em Int
newtype Width = Width { fromWidth :: Length }
newtype Height = Height { fromHeight :: Length }

instance Show Length where
  show (Px x) = show x ++ "px"
  show (Cm x) = show x ++ "cm"
  show (Em x) = show x ++ "em"

toPixels :: Length -> Int
toPixels (Px x) = x
toPixels _ = error "toPixels: wrong unit"

instance IsNode Root
instance IsNode Preambule
instance IsNode Document
instance IsNode Section
instance IsNode Subsection
instance IsNode Paragraph
instance IsNode UList
instance IsNode Item
instance IsNode Span
instance IsNode Leaf
instance IsNode HLink
instance IsNode Title
instance IsNode Image
instance IsNode Br
instance IsNode Hr
instance IsNode Table
instance IsNode Row
instance IsNode Col
instance IsNode HCol
instance IsNode a => IsNode (Div a)

class IsNode a => IsInline a

instance IsInline Image
instance IsInline Leaf
instance IsInline HLink
instance IsInline Span
instance IsInline Br
instance IsInline Hr

class IsNode a => IsBlock a

instance IsBlock Paragraph
instance IsBlock a => IsBlock (Div a)
instance IsBlock UList
instance IsBlock Table
instance IsBlock Hr

class IsNode a => IsBlockOrInline a

instance IsBlockOrInline Image
instance IsBlockOrInline Leaf
instance IsBlockOrInline HLink
instance IsBlockOrInline Span
instance IsBlockOrInline Br
instance IsBlockOrInline Hr
instance IsBlockOrInline Paragraph
instance IsBlockOrInline a => IsBlockOrInline (Div a)
instance IsBlockOrInline UList
instance IsBlockOrInline Table

instance Child Root Preambule
instance Child Root Document

instance Child Preambule Title

instance Child Title Leaf

instance Child a b => Child (Div a) b

instance Child Document Section
-- instance IsBlock a => Child Document a
instance Child Document Paragraph
instance Child Document UList
instance Child Document Table
instance Child Document Hr
instance a ~ Document => Child Document (Div a)

instance Child Section Subsection
-- instance IsBlock a => Child Section a
instance Child Section Paragraph
instance a ~ Section => Child Section (Div a)
instance Child Section UList
instance Child Section Table
instance Child Section Hr

instance IsBlock a => Child Subsection a

instance IsBlockOrInline a => Child Col a
instance IsBlockOrInline a => Child HCol a

instance IsInline a => Child Paragraph a
instance IsInline a => Child Span a
instance IsInline a => Child HLink a

instance IsBlockOrInline a => Child Item a

instance Child UList Item
instance Child Row Col
instance Child Row HCol
instance Child Table Row

instance IsAttribute Alt
instance IsAttribute Src
instance IsAttribute Height
instance IsAttribute Width
instance IsAttribute Style
instance IsAttribute ClassAttr
instance IsAttribute Name
instance IsAttribute Size
instance IsAttribute Rows
instance IsAttribute Cols

instance IsNode a => IsAttributeOf ClassAttr a
instance IsNode n => IsAttributeOf Style n
instance IsAttributeOf Alt Image
instance IsAttributeOf Src Image
instance IsAttributeOf Height Image
instance IsAttributeOf Width Image

class LeafTags t where
  charTag              :: Char -> t Leaf
  stringTag            :: String -> t Leaf
  strictByteStringTag  :: Strict.ByteString -> t Leaf
  lazyByteStringTag    :: Lazy.ByteString -> t Leaf

class AttributeTags t where
  altTag               :: t Alt
  srcTag               :: t Src
  widthTag             :: t Width
  heightTag            :: t Height
  classAttrTag         :: t ClassAttr
  nameTag              :: t Name
  sizeTag              :: t Size
  rowsTag              :: t Rows
  colsTag              :: t Cols
  styleTag             :: t Style

class (AttributeTags t, LeafTags t) => Tag t where
  rootTag              :: t Root
  preambuleTag         :: t Preambule
  documentTag          :: t Document
  sectionTag           :: Child Span a => TDoc t a -> t Section
  subsectionTag        :: Child Span a => TDoc t a -> t Subsection
  uListTag             :: t UList
  itemTag              :: t Item
  paragraphTag         :: t Paragraph
  spanTag              :: t Span
  hLinkTag             :: Url -> t HLink
  titleTag             :: t Title
  imageTag             :: t Image
  brTag                :: t Br
  hrTag                :: t Hr
  tableTag             :: t Table
  rowTag               :: t Row
  colTag               :: t Col
  hColTag              :: t HCol
  divTag               :: t (Div a)

{-
class Child a Leaf => HasLeaves a
instance HasLeaves Title
instance HasLeaves Paragraph
instance HasLeaves Span
instance HasLeaves HLink
instance HasLeaves Label
instance HasLeaves Col
instance HasLeaves HCol
instance HasLeaves Option
instance HasLeaves Textarea
-}
instance (LeafTags t, Child a Leaf) => ToChildren t Char               a where toChildren = toChildren . char
instance (LeafTags t, Child a Leaf) => ToChildren t Lazy.ByteString    a where toChildren = toChildren . lazyByteString
instance (LeafTags t, Child a Leaf) => ToChildren t Strict.ByteString  a where toChildren = toChildren . strictByteString

root :: forall t doc preambule. (Tag t, ToTDoc t preambule Preambule, ToTDoc t doc Document) => preambule -> doc -> TDoc t Root
root x y = tStar rootTag [ TChild (toTDoc x :: TDoc t Preambule)
                         , TChild (toTDoc y :: TDoc t Document) ]

char :: LeafTags t => Char -> TDoc t Leaf
char = tNullary . charTag

string :: LeafTags t => String -> TDoc t Leaf
string = tNullary . stringTag

instance (LeafTags t, b ~ Char, a ~ Leaf) => ToTDoc t [b] a where
  toTDoc = string

strictByteString :: LeafTags t => Strict.ByteString -> TDoc t Leaf
strictByteString = tNullary . strictByteStringTag

lazyByteString :: LeafTags t => Lazy.ByteString -> TDoc t Leaf
lazyByteString = tNullary . lazyByteStringTag

preambule :: Tag t => Star t Preambule
preambule = tStar preambuleTag

document :: Tag t => Star t Document
document = tStar documentTag

title :: Tag t => Star t Title
title = tStar titleTag

section :: forall a b t. (Tag t, Child Span a, ToTDoc t b a) => b -> Star t Section
section t = tStar (sectionTag (toTDoc t :: TDoc t a))

subsection :: forall a b t. (Tag t, Child Span a, ToTDoc t b a) => b -> Star t Subsection
subsection t = tStar (subsectionTag (toTDoc t :: TDoc t a))

div :: Tag t => Star t (Div a)
div = tStar divTag

ulist :: Tag t => Star t UList
ulist = tStar uListTag

-- | 'ulistQ' is a quick version of 'ulist' when all children
-- of a UList are homogeneous one can factor the building of
-- the Item nodes.
ulistQ :: (Tag t, Child Item a) => [TDoc t a] -> TDoc t UList
ulistQ = tStar uListTag . map item

item :: Tag t => Star t Item
item = tStar itemTag

table :: Tag t => Star t Table
table = tStar tableTag

col :: Tag t => Star t Col
col = tStar colTag

hcol :: Tag t => Star t HCol
hcol = tStar hColTag

row :: Tag t => Star t Row
row = tStar rowTag

-- since their is no 'instance Child Leaf X'
-- one cannot build a 'TNode attrs [x] :: Tag t => TDoc t Leaf'
-- but one can build a 'TNode attrs [] :: Tag t => TDoc t Leaf'

paragraph :: Tag t => Star t Paragraph
paragraph = tStar paragraphTag

para :: (Tag t, ToChildren t children Paragraph) => children -> TDoc t Paragraph
para = paragraph

spanDoc :: Tag t => Star t Span
spanDoc = tStar spanTag

spanDocCA :: Tag t => String -> Star t Span
spanDocCA ca = tStar spanTag ! [classAttr ca]

strong :: Tag t => Star t Span
strong = spanDocCA "strong"

small :: Tag t => Star t Span
small = spanDocCA "small"

big :: Tag t => Star t Span
big = spanDocCA "big"

italics :: Tag t => Star t Span
italics = spanDocCA "italics"

sub :: Tag t => Star t Span
sub = spanDocCA "sub"

sup :: Tag t => Star t Span
sup = spanDocCA "sup"

tt :: Tag t => Star t Span
tt = spanDocCA "tt"

bold :: Tag t => Star t Span
bold = spanDocCA "bold"

br :: Tag t => Nullary t Br
br = tNullary brTag

hr :: Tag t => Nullary t Hr
hr = tNullary hrTag

hlink :: Tag t => String -> Star t HLink
hlink url = tStar (hLinkTag (Url url))

style :: (AttributeTags t, IsAttributeOf Style a) => String -> AttributeOf t a
style = TAttr styleTag . Style

image :: Tag t => Nullary t Image
image = tNullary imageTag

src :: AttributeTags t => String -> AttributeOf t Image
src = TAttr srcTag . Src

width :: AttributeTags t => Length -> AttributeOf t Image
width = TAttr widthTag . Width

height :: AttributeTags t => Length -> AttributeOf t Image
height = TAttr heightTag . Height

alt :: AttributeTags t => String -> AttributeOf t Image
alt = TAttr altTag . Alt

name :: (AttributeTags t, IsAttributeOf Name a) => String -> AttributeOf t a
name = TAttr nameTag . Name

rows :: (AttributeTags t, IsAttributeOf Rows a) => Int -> AttributeOf t a
rows = TAttr rowsTag . Rows

cols :: (AttributeTags t, IsAttributeOf Cols a) => Int -> AttributeOf t a
cols = TAttr colsTag . Cols

size :: (AttributeTags t, IsAttributeOf Size a) => Int -> AttributeOf t a
size = TAttr sizeTag . Size

classAttr :: (AttributeTags t, IsNode a) => String -> AttributeOf t a
classAttr = TAttr classAttrTag . ClassAttr

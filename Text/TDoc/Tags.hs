{-# LANGUAGE TypeFamilies, ScopedTypeVariables, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Text.TDoc.Tags where

import Text.TDoc.Core
import Text.TDoc.Attributes
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy

data Root
instance IsNode Root
instance Child Root Preambule
instance Child Root Document
root :: forall t doc preambule. (Tags t, ToTDoc t preambule Preambule, ToTDoc t doc Document) => preambule -> doc -> TDoc t Root
root x y = tStar rootTag [ TChild (toTDoc x :: TDoc t Preambule)
                         , TChild (toTDoc y :: TDoc t Document) ]

data Preambule
instance IsNode Preambule
instance Child Preambule Title
preambule :: Tags t => Star t Preambule
preambule = tStar preambuleTag

data Image
instance IsNode Image
instance IsInline Image
instance IsBlockOrInline Image
instance IsAttributeOf Alt Image
instance IsAttributeOf Src Image
instance IsAttributeOf Height Image
instance IsAttributeOf Width Image
image :: Tags t => Nullary t Image
image = tNullary imageTag

data Document
instance IsNode Document
instance Child Document Section
-- instance IsBlock a => Child Document a
instance Child Document Paragraph
instance Child Document UList
instance Child Document Table
instance Child Document Hr
instance a ~ Document => Child Document (Div a)
document :: Tags t => Star t Document
document = tStar documentTag

data Subsection
instance IsNode Subsection
instance Child Section Subsection
instance IsBlock a => Child Subsection a
subsection :: forall a b t. (Tags t, Child Span a, ToTDoc t b a) => b -> Star t Subsection
subsection t = tStar (subsectionTag (toTDoc t :: TDoc t a))

data Section
instance IsNode Section
-- instance IsBlock a => Child Section a
instance Child Section Paragraph
instance a ~ Section => Child Section (Div a)
instance Child Section UList
instance Child Section Table
instance Child Section Hr
section :: forall a b t. (Tags t, Child Span a, ToTDoc t b a) => b -> Star t Section
section t = tStar (sectionTag (toTDoc t :: TDoc t a))

data Item
instance IsNode Item
instance IsBlockOrInline a => Child Item a
item :: Tags t => Star t Item
item = tStar itemTag

data UList
instance IsNode UList
instance IsBlock UList
instance IsBlockOrInline UList
instance Child UList Item
ulist :: Tags t => Star t UList
ulist = tStar uListTag

data Paragraph
instance IsNode Paragraph
instance IsBlock Paragraph
instance IsBlockOrInline Paragraph
instance IsInline a => Child Paragraph a
paragraph :: Tags t => Star t Paragraph
paragraph = tStar paragraphTag
para :: (Tags t, ToChildren t children Paragraph) => children -> TDoc t Paragraph
para = paragraph

data HLink
instance IsNode HLink
instance IsInline HLink
instance IsBlockOrInline HLink
instance IsInline a => Child HLink a
hlink :: Tags t => String -> Star t HLink
hlink url = tStar (hLinkTag (Url url))

data Title
instance IsNode Title
instance Child Title Leaf
title :: Tags t => Star t Title
title = tStar titleTag

data Br
instance IsNode Br
instance IsInline Br
instance IsBlockOrInline Br
br :: Tags t => Nullary t Br
br = tNullary brTag

data Hr
data Table
data Row
data Col
data HCol
data Div a
data Leaf
newtype Url = Url { fromUrl :: String }

instance IsNode Leaf
instance IsNode Hr
instance IsNode Table
instance IsNode Row
instance IsNode Col
instance IsNode HCol
instance IsNode a => IsNode (Div a)

class IsNode a => IsInline a

instance IsInline Leaf
instance IsInline Hr

class IsNode a => IsBlock a

instance IsBlock a => IsBlock (Div a)
instance IsBlock Table
instance IsBlock Hr

class IsNode a => IsBlockOrInline a

instance IsBlockOrInline Leaf
instance IsBlockOrInline Hr
instance IsBlockOrInline a => IsBlockOrInline (Div a)
instance IsBlockOrInline Table


instance Child a b => Child (Div a) b


instance IsBlockOrInline a => Child Col a
instance IsBlockOrInline a => Child HCol a



instance Child Row Col
instance Child Row HCol
instance Child Table Row

class LeafTags t where
  charTag              :: Char -> t Leaf
  stringTag            :: String -> t Leaf
  strictByteStringTag  :: Strict.ByteString -> t Leaf
  lazyByteStringTag    :: Lazy.ByteString -> t Leaf

class (SpanTag t
      ,AnchorTag t
      ,AttributeTags t, LeafTags t) => Tags t where
  rootTag              :: t Root
  preambuleTag         :: t Preambule
  documentTag          :: t Document
  sectionTag           :: Child Span a => TDoc t a -> t Section
  subsectionTag        :: Child Span a => TDoc t a -> t Subsection
  uListTag             :: t UList
  itemTag              :: t Item
  paragraphTag         :: t Paragraph
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

instance (LeafTags t, Child a Leaf) => ToChildren t Char               a where toChildren = toChildren . char
instance (LeafTags t, Child a Leaf) => ToChildren t Lazy.ByteString    a where toChildren = toChildren . lazyByteString
instance (LeafTags t, Child a Leaf) => ToChildren t Strict.ByteString  a where toChildren = toChildren . strictByteString


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

div :: Tags t => Star t (Div a)
div = tStar divTag

-- | 'ulistQ' is a quick version of 'ulist' when all children
-- of a UList are homogeneous one can factor the building of
-- the Item nodes.
ulistQ :: (Tags t, Child Item a) => [TDoc t a] -> TDoc t UList
ulistQ = tStar uListTag . map item

table :: Tags t => Star t Table
table = tStar tableTag

col :: Tags t => Star t Col
col = tStar colTag

hcol :: Tags t => Star t HCol
hcol = tStar hColTag

row :: Tags t => Star t Row
row = tStar rowTag

-- since their is no 'instance Child Leaf X'
-- one cannot build a 'TNode attrs [x] :: Tags t => TDoc t Leaf'
-- but one can build a 'TNode attrs [] :: Tags t => TDoc t Leaf'

hr :: Tags t => Nullary t Hr
hr = tNullary hrTag

{- SPAN -}

data Span
instance IsNode Span
instance IsInline Span
instance IsBlockOrInline Span
instance IsInline a => Child Span a
class    ClassAttrTag t => SpanTag t where spanTag :: t Span

spanDoc :: SpanTag t => Star t Span
spanDoc = tStar spanTag

spanDocCA :: SpanTag t => String -> Star t Span
spanDocCA ca = tStar spanTag ! [classAttr ca]

strong :: SpanTag t => Star t Span
strong = spanDocCA "strong"

small :: SpanTag t => Star t Span
small = spanDocCA "small"

big :: SpanTag t => Star t Span
big = spanDocCA "big"

italics :: SpanTag t => Star t Span
italics = spanDocCA "italics"

sub :: SpanTag t => Star t Span
sub = spanDocCA "sub"

sup :: SpanTag t => Star t Span
sup = spanDocCA "sup"

tt :: SpanTag t => Star t Span
tt = spanDocCA "tt"

bold :: SpanTag t => Star t Span
bold = spanDocCA "bold"

data Anchor
instance IsNode Anchor
instance IsInline Anchor
instance IsBlockOrInline Anchor
instance Child Anchor Span
class    AnchorTag t where anchorTag :: Identifier -> t Anchor
anchor :: AnchorTag t => Identifier -> Unary t Anchor
anchor i = tUnary (anchorTag i)
{-
anchor :: String -> TDoc Span -> TDoc Span
anchor nam body = spanDoc . rawHtml_ (undefined :: Span) $ X.anchor X.! [X.identifier nam] X.<< body
-}

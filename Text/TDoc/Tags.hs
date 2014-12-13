--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.Tags
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, ScopedTypeVariables, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, Rank2Types,
             FlexibleInstances, TemplateHaskell, TypeOperators,
             CPP #-}
#if MIN_VERSION_base(4,7,0)
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif
module Text.TDoc.Tags where

import Text.TDoc.Core
import Text.TDoc.TH (node,NodeOpt(..))
import Text.TDoc.Attributes
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy

$(node "Leaf" [Inline, BlockOrInline] [] [])
newtype Url = Url { fromUrl :: String }

class LeafTags t where
  charTag              :: Char -> t Leaf
  stringTag            :: String -> t Leaf
  strictByteStringTag  :: Strict.ByteString -> t Leaf
  lazyByteStringTag    :: Lazy.ByteString -> t Leaf

instance (LeafTags t, Leaf `IsChildOf` a) => ToChildren Char               t a where toChildren = toChildren . char
instance (LeafTags t, Leaf `IsChildOf` a) => ToChildren Lazy.ByteString    t a where toChildren = toChildren . lazyByteString
instance (LeafTags t, Leaf `IsChildOf` a) => ToChildren Strict.ByteString  t a where toChildren = toChildren . strictByteString

char :: LeafTags t => Char -> TDoc t Leaf
char = tNullary . charTag

string :: LeafTags t => String -> TDoc t Leaf
string = tNullary . stringTag

strictByteString :: LeafTags t => Strict.ByteString -> TDoc t Leaf
strictByteString = tNullary . strictByteStringTag

lazyByteString :: LeafTags t => Lazy.ByteString -> TDoc t Leaf
lazyByteString = tNullary . lazyByteStringTag

instance (LeafTags t, a ~ Leaf) => ToTDoc Char t a where
  toTDoc = char

instance (LeafTags t, b ~ Char, a ~ Leaf) => ToTDoc [b] t a where
  toTDoc = string

instance (LeafTags t, a ~ Leaf) => ToTDoc Strict.ByteString t a where
  toTDoc = strictByteString

instance (LeafTags t, a ~ Leaf) => ToTDoc Lazy.ByteString t a where
  toTDoc = lazyByteString

--

$(node "Paragraph" [Block] [] [])
instance IsInline a => IsChildOf a Paragraph
paragraph, para :: ParagraphTag t => Star t Paragraph
paragraph = tStar paragraphTag
para = paragraph

$(node "Title" [] [] [''Leaf])
title :: TitleTag t => Star t Title
title = tStar titleTag

$(node "Br" [Block, Inline] [] [])
br :: BrTag t => Nullary t Br
br = tNullary brTag

$(node "Hr" [Block, Inline] [] [])
hr :: HrTag t => Nullary t Hr
hr = tNullary hrTag

$(node "Col" [] [] [])
instance IsBlockOrInline a => IsChildOf a Col
col :: ColTag t => Star t Col
col = tStar colTag

$(node "HCol" [] [] [])
instance IsBlockOrInline a => IsChildOf a HCol
hcol :: HColTag t => Star t HCol
hcol = tStar hColTag

$(node "Row" [] [] [''Col, ''HCol])
row :: RowTag t => Star t Row
row = tStar rowTag

$(node "Table" [Block] [] [''Row])
table :: TableTag t => Star t Table
table = tStar tableTag

$(node "Item" [] [] [])
instance IsBlockOrInline a => IsChildOf a Item
item :: ItemTag t => Star t Item
item = tStar itemTag

--

$(node "UList" [Block] [] [''Item])
ulist :: UListTag t => Star t UList
ulist = tStar uListTag

-- | 'ulistQ' is a quick version of 'ulist' when all children
-- of a UList are homogeneous one can factor the building of
-- the Item nodes.
ulistQ :: (UListTag t, ItemTag t, a `IsChildOf` Item) => [TDoc t a] -> TDoc t UList
ulistQ = ulist . map item

--

$(node "Span" [NoTag, Inline] [] [])
instance IsInline a => IsChildOf a Span
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

--

data Div a
class DivTag t where divTag :: t (Div a)
div :: DivTag t => Star t (Div a)
div = tStar divTag
instance IsNode a => IsNode (Div a)
instance IsBlock a => IsBlock (Div a)
instance IsBlockOrInline a => IsBlockOrInline (Div a)
instance IsChildOf b a => IsChildOf b (Div a)

$(node "Subsection" [NoTag] [] [])
instance IsBlock a => IsChildOf a Subsection
class SubsectionTag t where
  subsectionTag :: (a `IsChildOf` Span) => TDoc t a -> t Subsection
subsection :: forall a b t. (SubsectionTag t, a `IsChildOf` Span, ToTDoc b t a) => b -> Star t Subsection
subsection t = tStar (subsectionTag (toTDoc t :: TDoc t a))

$(node "Section" [NoTag] [] [''Paragraph, ''UList, ''Table, ''Hr, ''Subsection])
-- instance IsBlock a => IsChildOf a Section
instance a ~ Section => IsChildOf (Div a) Section
class SectionTag t where
  sectionTag :: (a `IsChildOf` Span) => TDoc t a -> t Section
section :: forall a b t. (SectionTag t, a `IsChildOf` Span, ToTDoc b t a) => b -> Star t Section
section t = tStar (sectionTag (toTDoc t :: TDoc t a))

$(node "HLink" [NoTag, Inline] [''TitleAttr] [])
instance IsInline a => IsChildOf a HLink
class HLinkTag t where hLinkTag :: Url -> t HLink
hlink :: HLinkTag t => String -> Star t HLink
hlink url = tStar (hLinkTag (Url url))

-- TODO in HTML5 <a> is supposed to work on blocks too.
$(node "Anchor" [NoTag, Inline] [''Href] [''Span])
class AnchorTag t where anchorTag :: t Anchor
anchor :: AnchorTag t => Unary t Anchor
anchor = tUnary anchorTag

$(node "Image" [Inline] [''Alt, ''Src, ''Height, ''Width] [])
image :: ImageTag t => Nullary t Image
image = tNullary imageTag

$(node "Preambule" [] [] [''Title])
preambule :: PreambuleTag t => Star t Preambule
preambule = tStar preambuleTag

-- instance IsBlock a => IsChildOf a Document
$(node "Document" [] [] [''Section, ''Paragraph, ''UList, ''Table, ''Hr])
instance a ~ Document => IsChildOf (Div a) Document
document :: DocumentTag t => Star t Document
document = tStar documentTag

$(node "Root" [] [] [''Preambule, ''Document])
root :: forall t doc preambule. (RootTag t, ToTDoc preambule t Preambule, ToTDoc doc t Document) => preambule -> doc -> TDoc t Root
root x y = tStar rootTag [ Child (toTDoc x :: TDoc t Preambule)
                         , Child (toTDoc y :: TDoc t Document) ]

class (AttributeTags t
      ,LeafTags t
      ,ParagraphTag t
      ,TitleTag t
      ,BrTag t
      ,HrTag t
      ,ColTag t
      ,HColTag t
      ,RowTag t
      ,TableTag t
      ,ItemTag t
      ,UListTag t
      ,SpanTag t
      ,DivTag t
      ,SubsectionTag t
      ,SectionTag t
      ,HLinkTag t
      ,AnchorTag t
      ,ImageTag t
      ,PreambuleTag t
      ,DocumentTag t
      ,RootTag t
      ) => Tags t where

-- since their is no 'instance IsChildOf X Leaf'
-- one cannot build a 'TNode attrs [x] :: Tags t => TDoc t Leaf'
-- but one can build a 'TNode attrs [] :: Tags t => TDoc t Leaf'


{-# LANGUAGE Rank2Types, TypeFamilies, ScopedTypeVariables, GADTs, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,

 UndecidableInstances

   #-}
module Text.TDoc where

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict (HTML, Html, HtmlAttr, toHtml)
{-
import Data.Monoid hiding (Any)
import Data.Char
import Data.List
import Data.Either
-}
import Data.Maybe
import Control.Monad.Writer hiding (Any)
import Control.Monad.Identity
import Control.Arrow (second)
import Control.Exception (assert)

{-

TODO:

- some attributes are mandatory, how to handle this ?

-}

-- this class allow to close some type-classes
-- class HiddenClass a

class IsNode node

class IsAttribute attr

class (IsNode father, IsNode child) => Child father child

-- data Any
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
data RawHtml
data Table
data Row
data Col
data HCol
data Div
newtype Leaf = Leaf String
newtype Style = Style { fromStyle :: String } -- put something more typeful
newtype Url = Url { fromUrl :: String }
newtype Alt = Alt { fromAlt :: String }
newtype Src = Src { fromSrc :: String }
newtype ClassAttr = ClassAttr { fromClassAttr :: String }
data Size = Px Int
          | Cm Int
          | Em Int
newtype Width = Width { getWidth :: Size }
newtype Height = Height { getHeight :: Size }

instance Show Size where
  show (Px x) = show x ++ "px"
  show (Cm x) = show x ++ "cm"
  show (Em x) = show x ++ "em"

toPixels :: Size -> Int
toPixels (Px x) = x
toPixels _ = error "toPixels: wrong unit"

data Tag tag where
  -- AnyTag        :: Tag Any
  RootTag       :: Tag Root
  PreambuleTag  :: Tag Preambule
  DocumentTag   :: Tag Document
  SectionTag    :: Child Span a => TDoc a -> Tag Section
  SubsectionTag :: Child Span a => TDoc a -> Tag Subsection
  UListTag      :: Tag UList
  ItemTag       :: Tag Item
  ParagraphTag  :: Tag Paragraph
  SpanTag       :: Tag Span
  LeafTag       :: String -> Tag Leaf
  HLinkTag      :: Url -> Tag HLink
  TitleTag      :: Tag Title
  ImageTag      :: Tag Image
  BrTag         :: Tag Br
  HrTag         :: Tag Hr
  RawHtmlTag    :: Html -> Tag RawHtml
  TableTag      :: Tag Table
  RowTag        :: Tag Row
  ColTag        :: Tag Col
  HColTag       :: Tag HCol
  DivTag        :: Tag Div
  
  StyleTag      :: Tag Style
  AltTag        :: Tag Alt
  SrcTag        :: Tag Src
  WidthTag      :: Tag Width
  HeightTag     :: Tag Height
  ClassAttrTag  :: Tag ClassAttr
  -- UrlTag        :: Tag Url

-- instance IsNode Any
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
instance IsNode RawHtml
instance IsNode Table
instance IsNode Row
instance IsNode Col
instance IsNode HCol
instance IsNode Div

class IsNode a => IsInline a

instance IsInline Image
instance IsInline Leaf
instance IsInline HLink
instance IsInline Span
instance IsInline Br
instance IsInline Hr
instance IsInline RawHtml

class IsNode a => IsBlock a

instance IsBlock Paragraph
instance IsBlock Div
instance IsBlock UList
instance IsBlock Table
instance IsBlock RawHtml
instance IsBlock Hr

class IsNode a => IsBlockOrInline a

instance IsBlockOrInline Image
instance IsBlockOrInline Leaf
instance IsBlockOrInline HLink
instance IsBlockOrInline Span
instance IsBlockOrInline Br
instance IsBlockOrInline Hr
instance IsBlockOrInline RawHtml
instance IsBlockOrInline Paragraph
instance IsBlockOrInline Div
instance IsBlockOrInline UList
instance IsBlockOrInline Table

-- instance IsNode a => Child Any a
-- instance IsNode a => Child a RawHtml

instance Child Root Preambule
instance Child Root Document
instance Child Root RawHtml

instance Child Preambule Title
instance Child Preambule RawHtml

instance Child Title Leaf
instance Child Title RawHtml

instance Child Document Section
-- instance IsBlock a => Child Document a
instance Child Document Paragraph
instance Child Document Div
instance Child Document UList
instance Child Document Table
instance Child Document RawHtml
instance Child Document Hr

instance Child Section Subsection
-- instance IsBlock a => Child Section a
instance Child Section Paragraph
instance Child Section Div
instance Child Section UList
instance Child Section Table
instance Child Section RawHtml
instance Child Section Hr

instance IsBlock a => Child Subsection a

instance IsBlockOrInline a => Child Col a
instance IsBlockOrInline a => Child HCol a

instance IsInline a => Child Paragraph a
instance IsInline a => Child Span a
instance IsInline a => Child HLink a

instance IsBlockOrInline a => Child Item a

instance (Child a Div, Child a b) => Child Div b
instance Child UList Item
instance Child Row Col
instance Child Row HCol
instance Child Table Row

class (IsAttribute attr, IsNode node) => IsAttributeOf attr node

-- instance IsAttribute Url
instance IsAttribute Alt
instance IsAttribute Src
instance IsAttribute Height
instance IsAttribute Width
instance IsAttribute Style
instance IsAttribute ClassAttr

instance IsNode a => IsAttributeOf ClassAttr a
instance IsNode n => IsAttributeOf Style n
instance IsAttributeOf Alt Image
instance IsAttributeOf Src Image
instance IsAttributeOf Height Image
instance IsAttributeOf Width Image

data TDoc tag where
  TNode   :: Tag fatherTag -> [AttributeOf fatherTag] ->
             [TChildOf fatherTag] -> TDoc fatherTag

data TChildOf tag where
  TChild :: Child fatherTag childTag => TDoc childTag -> TChildOf fatherTag

type PutM a = Writer [a] ()

type TDocMaker node = forall children. ToChildren children node => children -> TDoc node

data AttributeOf nodeTag where
  TAttr :: IsAttributeOf attrTag nodeTag => Tag attrTag -> attrTag -> AttributeOf nodeTag

-- try to use this
-- newtype AttributesMap nodeTag = AttrMap { getAttrMap :: Map AnyTag (AttributeOf nodeTag) }


type AttributesOf nodeTag = [AttributeOf nodeTag] -- AttributesMap nodeTag

infixl 8 !

class AddAttrs a b where
  (!) :: a -> AttributesOf b -> a

instance a ~ b => AddAttrs (TDoc a) b where
  (TNode tag attrs children) ! attrs' = TNode tag (attrs++attrs') children

instance AddAttrs b c => AddAttrs (a -> b) c where
  (f ! attrs) x = f x ! attrs

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

class ToTDoc a b where
  toTDoc :: a -> TDoc b

instance (b ~ Char, a ~ Leaf) => ToTDoc [b] a where
  toTDoc = string

instance a ~ b => ToTDoc (TDoc a) b where
  toTDoc = id

class ToChildren a father where
  toChildren :: a -> [TChildOf father] 

instance ToChildren a b => ToChildren [a] b where
  toChildren = concatMap toChildren

instance ToChildren () b where
  toChildren () = []

instance a ~ b => ToChildren (TChildOf a) b where
  toChildren = (:[])

instance Child b a => ToChildren (TDoc a) b where
  toChildren = (:[]) . TChild

instance (Monad m, ToChildren [a] b, w ~ (), m ~ Identity) => ToChildren (WriterT [a] m w) b where
  toChildren = toChildren . execWriter

instance Child a Leaf => ToChildren Char a where
  toChildren = toChildren . char

class FromTDoc tag a where
  fromTDoc :: TDoc tag -> a

instance a ~ b => FromTDoc a (TDoc b) where
  fromTDoc = id

instance Child b a => FromTDoc a (TChildOf b) where
  fromTDoc = TChild

instance (FromTDoc tag a) => FromTDoc tag [a] where
  fromTDoc = (:[]) . fromTDoc

instance (Monad m, FromTDoc tag a, Monoid a, b ~ ()) => FromTDoc tag (WriterT a m b) where
  fromTDoc = tell . fromTDoc

infixr 7 <<
infixr 2 +++

(+++) :: (ToChildren a tag, ToChildren b tag) => a -> b -> [TChildOf tag]
a +++ b = toChildren a <> toChildren b

-- | This operator is an infix sugar for 'put'
-- @paragraph << do ...@ is equal to @put $ paragraph $ do ...@.
(<<) :: (Child b a) => (c -> TDoc a) -> c -> PutM (TChildOf b)
(<<) f = put . f

put :: ToChildren children fatherTag => children -> PutM (TChildOf fatherTag)
put = tell . toChildren

tNode :: ToChildren a fatherTag => Tag fatherTag -> [AttributeOf fatherTag] ->
         a -> TDoc fatherTag
tNode tag attrs = TNode tag attrs . toChildren

root :: (ToTDoc preambule Preambule, ToTDoc doc Document) => preambule -> doc -> TDoc Root
root x y = TNode RootTag [] [ TChild (toTDoc x :: TDoc Preambule)
                            , TChild (toTDoc y :: TDoc Document) ]

preambule :: TDocMaker Preambule
preambule = tNode PreambuleTag []

document :: TDocMaker Document
document = tNode DocumentTag []

title :: TDocMaker Title
title = tNode TitleTag []

section :: forall a b. (Child Span a, ToTDoc b a) => b -> TDocMaker Section
section t = tNode (SectionTag (toTDoc t :: TDoc a)) []

subsection :: forall a b. (Child Span a, ToTDoc b a) => b -> TDocMaker Subsection
subsection t = tNode (SubsectionTag (toTDoc t :: TDoc a)) []

div :: AttributesOf Div -> TDocMaker Div
div eta = tNode DivTag eta

ulist :: TDocMaker UList
ulist = tNode UListTag []

-- | 'ulistQ' is a quick version of 'ulist' when all children
-- of a UList are homogeneous one can factor the building of
-- the Item nodes.
ulistQ :: Child Item a => [TDoc a] -> TDoc UList
ulistQ = tNode UListTag [] . map item

item :: TDocMaker Item
item = tNode ItemTag []

table :: TDocMaker Table
table = tNode TableTag []

col :: TDocMaker Col
col = tNode ColTag []

hcol :: TDocMaker HCol
hcol = tNode HColTag []

row :: TDocMaker Row
row = tNode RowTag []

-- since their is no 'instance Child Leaf X'
-- one cannot build a 'TNode attrs [x] :: TDoc Leaf'
-- but one can build a 'TNode attrs [] :: TDoc Leaf' 

paragraph :: TDocMaker Paragraph
paragraph = tNode ParagraphTag []

p :: TDocMaker Paragraph
p = paragraph

char :: Char -> TDoc Leaf
char c = TNode (LeafTag [c]) [] []

string :: String -> TDoc Leaf
string s = TNode (LeafTag s) [] []

rawHtml :: Html -> TDoc RawHtml
rawHtml h = TNode (RawHtmlTag h) [] []

spanDoc :: TDocMaker Span
spanDoc = tNode SpanTag []

strong :: TDocMaker Span
strong = tNode SpanTag [classAttr "strong"]

small :: TDocMaker Span
small = tNode SpanTag [classAttr "small"]

big :: TDocMaker Span
big = tNode SpanTag [classAttr "big"]

italics :: TDocMaker Span
italics = tNode SpanTag [classAttr "italics"]

sub :: TDocMaker Span
sub = tNode SpanTag [classAttr "sub"]

sup :: TDocMaker Span
sup = tNode SpanTag [classAttr "sup"]

teletype :: TDocMaker Span
teletype = tNode SpanTag [classAttr "teletype"]

bold :: TDocMaker Span
bold = tNode SpanTag [classAttr "bold"]

br :: TDoc Br
br = TNode BrTag [] []

hr :: TDoc Hr
hr = TNode HrTag [] []

hlink :: String -> TDocMaker HLink
hlink url = tNode (HLinkTag (Url url)) []

style :: forall a. IsAttributeOf Style a => String -> AttributeOf a
style = TAttr StyleTag . Style

image :: AttributesOf Image -> TDoc Image
image attrs = TNode ImageTag attrs []

src :: String -> AttributeOf Image
src = TAttr SrcTag . Src

width :: Size -> AttributeOf Image
width = TAttr WidthTag . Width

height :: Size -> AttributeOf Image
height = TAttr HeightTag . Height

alt :: String -> AttributeOf Image
alt = TAttr AltTag . Alt

classAttr :: IsNode a => String -> AttributeOf a
classAttr = TAttr ClassAttrTag . ClassAttr

-- instance HTML (TDoc Root) where toHtml = renderTDocHtml

instance IsNode a => HTML (TDoc a) where toHtml = renderTDocHtml

instance HTML (TChildOf fatherTag) where
  toHtml (TChild x) = renderTDocHtml x

lookupClassAttr :: IsAttributeOf ClassAttr nodeTag => AttributesOf nodeTag -> Maybe (String, AttributesOf nodeTag)
lookupClassAttr (TAttr ClassAttrTag (ClassAttr t) : attrs) = Just (t, attrs)
lookupClassAttr (attr : attrs) = fmap (second (attr:)) $ lookupClassAttr attrs
lookupClassAttr [] = Nothing

renderTDocHtml :: forall nodeTag . IsNode nodeTag => TDoc nodeTag -> Html
renderTDocHtml (TNode tag attrs children) = f tag
  where f :: IsNode nodeTag => Tag nodeTag -> Html
        f RootTag       = toHtml children
        f PreambuleTag  = X.header X.! map commonAttr attrs X.<< children
        f TitleTag      = X.thetitle X.! map commonAttr attrs X.<< children
        f DocumentTag   = X.body X.! map commonAttr attrs X.<< children
        f (SectionTag x)= heading X.h1 x
        f (SubsectionTag x) = heading X.h2 x
        f UListTag      = X.ulist X.! map commonAttr attrs X.<< children
        f ItemTag       = X.li X.! map commonAttr attrs X.<< children
        f ParagraphTag  = X.p X.! map commonAttr attrs X.<< children
        f DivTag        = X.thediv X.! map commonAttr attrs X.<< children
        f TableTag      = X.table X.! map commonAttr attrs X.<< children
        f ColTag        = X.td X.! map commonAttr attrs X.<< children
        f HColTag       = X.th X.! map commonAttr attrs X.<< children
        f RowTag        = X.tr X.! map commonAttr attrs X.<< children
        f (LeafTag x)   = assert (null children) $ -- since there is no Child instance for Leaf
                          assert (null attrs) $
                          toHtml x
        f SpanTag       = genSpan (lookupClassAttr attrs)
        f (HLinkTag url)= toHtml $ X.hotlink (getUrl url) X.! map hlinkAttr attrs X.<< children
        f ImageTag      = assert (null children) $ X.image X.! map imageAttr attrs
        f BrTag         = assert (null children) $ X.br X.! map commonAttr attrs
        f HrTag         = assert (null children) $ X.hr X.! map commonAttr attrs
        f (RawHtmlTag h)= assert (null children) $ assert (null attrs) h
        f ClassAttrTag  = error "impossible"
        f AltTag        = error "impossible"
        f StyleTag      = error "impossible"
        f SrcTag        = error "impossible"
        f WidthTag      = error "impossible"
        f HeightTag     = error "impossible"

        heading :: Child Span a => (Html -> Html) -> TDoc a -> Html
        heading hN child = hN {-X.! map commonAttr attrs-} X.<< child X.+++ children

        genSpan :: nodeTag ~ Span => Maybe (String, AttributesOf nodeTag) -> Html
        genSpan (Just ("strong", attrs')) = X.strong X.! map commonAttr attrs' X.<< children
        genSpan (Just ("italics", attrs')) = X.italics X.! map commonAttr attrs' X.<< children
        genSpan (Just ("teletype", attrs')) = X.tt X.! map commonAttr attrs' X.<< children
        genSpan (Just ("small", attrs')) = X.small X.! map commonAttr attrs' X.<< children
        genSpan (Just ("big", attrs')) = X.big X.! map commonAttr attrs' X.<< children
        genSpan (Just ("sub", attrs')) = X.sub X.! map commonAttr attrs' X.<< children
        genSpan (Just ("sup", attrs')) = X.sup X.! map commonAttr attrs' X.<< children
        genSpan (Just ("bold", attrs')) = X.bold X.! map commonAttr attrs' X.<< children
        genSpan _  | null attrs = toHtml children
                   | otherwise  = X.thespan X.! map commonAttr attrs X.<< children

        commonAttr :: IsNode a => AttributeOf a -> HtmlAttr
        commonAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        commonAttr (TAttr StyleTag (Style s)) = X.thestyle s
        commonAttr _ = error "commonAttr: bug"

        hlinkAttr :: AttributeOf HLink -> HtmlAttr
        hlinkAttr = undefined

        imageAttr :: AttributeOf Image -> HtmlAttr
        imageAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        imageAttr (TAttr StyleTag (Style x))   = X.thestyle x
        imageAttr (TAttr AltTag (Alt a))       = X.alt a
        imageAttr (TAttr SrcTag (Src a))       = X.src a
        imageAttr (TAttr WidthTag (Width w))   = X.width . show . toPixels $ w
        imageAttr (TAttr HeightTag (Height h)) = X.height . show . toPixels $ h
        imageAttr _ = error "imageAttr: bug"

ex :: IO ()
ex = putStr
     $ X.prettyHtml
     $ toHtml
     $ root
        (preambule $ title "t")
        $ document $ do
            section "s1" <<
              subsection "ss1" << do
                paragraph << "p1"
                ulist << do
                  item << paragraph "a"
                  item << paragraph << do
                    put "b"
                    put "c"
                paragraph << "p1"
            section "s2" <<
              subsection "ss2" << do
                paragraph << do
                  put "p2a"
                  put br
                  put "p2b"
                paragraph << ["p3a", "p3b"]
                put hr
                paragraph << string "p4"
                put $ paragraph ["p5a", "p5b"]
            section "s3" << ()
            put hr
            section "s4" << subsection "ss4" << paragraph << "p5"
            section "s5" << [hr,hr]

--end
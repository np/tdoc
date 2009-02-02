{-# LANGUAGE Rank2Types, TypeFamilies, ScopedTypeVariables, GADTs, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,

 UndecidableInstances

   #-}
module Text.TDoc where

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict (HTML, Html, HtmlAttr, (+++), toHtml)
{-
import Data.Monoid hiding (Any)
import Data.Char
import Data.List
import Data.Either
-}
import Data.Maybe
import Control.Monad.Writer hiding (Any)
import Control.Arrow (second)
import Control.Exception (assert)

{-

TODO:

- some attributes are mandatory (href of <a>, content of <leaf>), how to handle this ?

-}

newtype FrenchQuote = FrQ { frQ :: String }

instance HTML FrenchQuote where
  toHtml = toHtml . frQ

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
data Leaf
data Title
data Image
data Br
data RawHtml
newtype Content = Content { getContent :: FrenchQuote }
newtype Style = Style { getStyle :: String } -- put something more typeful
newtype Url = Url { getUrl :: String }
newtype Alt = Alt { getAlt :: String }
newtype Src = Src { getSrc :: String }
newtype ClassAttr = ClassAttr { getClassAttr :: String }
data Size = Px Int
          | Cm Int
          | Em Int
newtype Width = Width { getWidth :: Size }
newtype Height = Height { getHeight :: Size }

instance Show Size where
  show (Px x) = show x ++ "px"
  show (Cm x) = show x ++ "cm"
  show (Em x) = show x ++ "em"

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
  LeafTag       :: Tag Leaf
  HLinkTag      :: Url -> Tag HLink
  TitleTag      :: Tag Title
  ImageTag      :: Tag Image
  BrTag         :: Tag Br
  RawHtmlTag    :: Html -> Tag RawHtml
  
  ContentTag    :: Tag Content
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
instance IsNode RawHtml

-- instance IsNode a => Child Any a
instance IsNode a => Child a RawHtml
instance Child Root Preambule
instance Child Root Document
instance Child Document Section
instance Child Document Paragraph
instance Child Document Image
instance Child Document UList
instance Child Preambule Title
instance Child Title Leaf
instance Child Section Subsection
instance Child Section Paragraph
instance Child Section Image
instance Child Section UList
instance Child Subsection Paragraph
instance Child Subsection Image
instance Child Subsection UList
instance Child UList Item
instance Child Item Paragraph
instance Child Item Image
instance Child Item HLink
instance Child Item Leaf
instance Child Item Span
instance Child Item Br
instance Child Paragraph Leaf
instance Child Paragraph HLink
instance Child Paragraph Span
instance Child Paragraph Br
instance Child Paragraph Image
instance Child Span Span
instance Child Span HLink
instance Child Span Leaf
instance Child Span Br
instance Child Span Image
instance Child HLink Leaf
instance Child HLink Span
instance Child HLink Br
instance Child HLink Image
instance Child Image Leaf
instance Child Image HLink
instance Child Image Span
instance Child Image Br

class (IsAttribute attr, IsNode node) => IsAttributeOf attr node

instance IsAttribute Content
-- instance IsAttribute Url
instance IsAttribute Alt
instance IsAttribute Src
instance IsAttribute Height
instance IsAttribute Width
instance IsAttribute Style
instance IsAttribute ClassAttr

instance IsNode a => IsAttributeOf ClassAttr a
instance IsNode n => IsAttributeOf Style n
instance IsAttributeOf Content Leaf
instance IsAttributeOf Alt Image
instance IsAttributeOf Src Image
instance IsAttributeOf Height Image
instance IsAttributeOf Width Image

-- instance IsAttributeOf Url HLink

data TDoc tag where
  TNode   :: Tag fatherTag -> [AttributeOf fatherTag] ->
             [TChildOf fatherTag] -> TDoc fatherTag

data TChildOf tag where
  TChild :: Child fatherTag childTag => TDoc childTag -> TChildOf fatherTag

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
  f ! attrs = \x -> f x ! attrs

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

root :: TDoc Preambule -> TDoc Document -> TDoc Root
root x y = TNode RootTag [] [TChild x, TChild y]

preambule :: TGenDoc1 Preambule
preambule = TNode PreambuleTag []

document :: TGenDoc1 Document
document = TNode DocumentTag []

title :: TGenDoc1 Title
title = TNode TitleTag []

class ToTDoc a b where
  toTDoc :: a -> TDoc b

{-
instance Child b Leaf => ToTDoc FrenchQuote b where
  toTDoc = leaf
-}
instance a ~ Leaf => ToTDoc FrenchQuote a where toTDoc = leaf
-- instance ToTDoc FrenchQuote Span where toTDoc = spanDoc . (:[]) . TChild . leaf
-- instance ToTDoc FrenchQuote Paragraph where toTDoc = leaf

instance a ~ b => ToTDoc (TDoc a) b where
  toTDoc = id

class ToChildren a father where
  toChildren :: a -> [TChildOf father] 

instance ToChildren a b => ToChildren [a] b where
  toChildren = concatMap toChildren

instance ToChildren () b where
  toChildren () = []

{-
instance a ~ b => ToChildren [TChildOf a] b where
  toChildren = id
-}

instance a ~ b => ToChildren (TChildOf a) b where
  toChildren = (:[])

instance Child b a => ToChildren (TDoc a) b where
  toChildren = (:[]) . TChild

{-
instance Child b a => ToChildren [TDoc a] b where
  toChildren = map TChild
-}

instance ToChildren [a] b => ToChildren (Writer [a] ()) b where
  toChildren = toChildren . execWriter

instance ToChildren FrenchQuote Paragraph where toChildren = toChildren . put . leaf
instance ToChildren FrenchQuote HLink where toChildren = toChildren . put . leaf
instance ToChildren FrenchQuote Span where toChildren = toChildren . put . leaf
instance ToChildren FrenchQuote Image where toChildren = toChildren . put . leaf
instance ToChildren FrenchQuote Title where toChildren = toChildren . put . leaf

{-
instance Child b Leaf => ToChildren FrenchQuote b where
  toChildren = toChildren . leaf
-}

{-
class FromTDoc tag a where
  fromTDoc :: TDoc tag -> a

instance a ~ b => FromTDoc a (TDoc b) where
  fromTDoc = id

instance (FromTDoc tag a) => FromTDoc tag (Writer [a] ()) where
  fromTDoc x = tell [fromTDoc x]
-}

type TGen a = Writer [a] ()
type TGenChildrenOf a = TGen (TChildOf a)
type TGenTDoc a = forall b. Child b a => TGenChildrenOf b
type TGenDoc father = forall grandfather a. (Child grandfather father, ToChildren a father) => a -> TGenChildrenOf grandfather
type TGenDoc1 a = [TChildOf a] -> TDoc a

infixr 0 <<
infixr 0 ^<<
infixr 0 <<^

(<<) :: (Child c a, ToChildren b a) => TGenDoc1 a -> b -> TGenChildrenOf c
(<<) f = put . f . toChildren

(^<<) :: (ToChildren b a) => TGenDoc1 a -> b -> TDoc a
(^<<) f = f . toChildren

(<<^) :: (Child b a) => TGenDoc1 a -> [TChildOf a] -> TGenChildrenOf b
(<<^) f = put . f

put :: Child fatherTag currentTag => TDoc currentTag -> TGenChildrenOf fatherTag
put = tell . (:[]) . TChild

section :: forall a b. (Child Span a, ToTDoc b a) => b -> TGenDoc1 Section
section t = TNode (SectionTag (toTDoc t :: TDoc a)) []

subsection :: forall a b. (Child Span a, ToTDoc b a) => b -> TGenDoc1 Subsection
subsection t = TNode (SubsectionTag (toTDoc t :: TDoc a)) []

ulist :: TGenDoc1 UList
ulist = TNode UListTag []

item :: TGenDoc1 Item
item = TNode ItemTag []

-- since their is no 'instance Child Leaf X'
-- one cannot build a 'TNode attrs [x] :: TDoc Leaf'
-- but one can build a 'TNode attrs [] :: TDoc Leaf' 

paragraph :: TGenDoc1 Paragraph
paragraph = TNode ParagraphTag []

p :: TGenDoc1 Paragraph
p = paragraph

leaf :: FrenchQuote -> TDoc Leaf
leaf c = TNode LeafTag [content c] []

rawHtml :: Html -> TDoc RawHtml
rawHtml h = TNode (RawHtmlTag h) [] []

spanDoc :: TGenDoc1 Span
spanDoc = TNode SpanTag []

strong :: TGenDoc1 Span
strong = TNode SpanTag [classAttr "strong"]

br :: TDoc Br
br = TNode BrTag [] []

hlink :: String -> TGenDoc1 HLink
hlink url = TNode (HLinkTag (Url url)) []

style :: forall a. IsAttributeOf Style a => String -> AttributeOf a
style = TAttr StyleTag . Style

image :: AttributesOf Image -> TDoc Image
image attrs = TNode ImageTag attrs []

content :: forall a. IsAttributeOf Content a => FrenchQuote -> AttributeOf a
content = TAttr ContentTag . Content

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

instance HTML a => HTML (Writer a ()) where
  toHtml = toHtml . execWriter

lookupContent :: IsAttributeOf Content nodeTag => AttributesOf nodeTag -> Maybe FrenchQuote
lookupContent (TAttr ContentTag (Content t) : _attrs) = Just t
lookupContent (_ : attrs) = lookupContent attrs
lookupContent [] = Nothing

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
        f LeafTag       = assert (null children) $ -- since there is no Child instance for Leaf
                          assert (length attrs == 1) $
                          let x = lookupContent attrs in
                          assert (isJust x) (toHtml $ fromJust x)
        f SpanTag       = genSpan (lookupClassAttr attrs)
        f (HLinkTag url)= toHtml $ X.hotlink (getUrl url) X.! map hlinkAttr attrs X.<< children
        f ImageTag      = assert (null children) $ X.image X.! map imageAttr attrs
        f BrTag         = assert (null children) $ X.br X.! map commonAttr attrs
        f (RawHtmlTag h)= assert (null children) $ assert (null attrs) h
        f ClassAttrTag  = error "impossible"
        f ContentTag    = error "impossible"
        f AltTag        = error "impossible"
        f StyleTag      = error "impossible"
        f SrcTag        = error "impossible"
        f WidthTag      = error "impossible"
        f HeightTag     = error "impossible"
        -- f UrlTag        = error "impossible"

        heading :: Child Span a => (Html -> Html) -> TDoc a -> Html
        heading hN child = hN {-X.! map commonAttr attrs-} X.<< child +++ children

        genSpan :: nodeTag ~ Span => Maybe (String, AttributesOf nodeTag) -> Html
        genSpan (Just ("strong", attrs')) = X.strong X.! map commonAttr attrs' X.<< children
        genSpan _  | null attrs = toHtml children
                   | otherwise  = X.thespan X.! map commonAttr attrs X.<< children

        commonAttr :: IsNode a => AttributeOf a -> HtmlAttr
        commonAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        commonAttr (TAttr StyleTag (Style s)) = X.thestyle s

        hlinkAttr :: AttributeOf HLink -> HtmlAttr
        hlinkAttr = undefined

        imageAttr :: AttributeOf Image -> HtmlAttr
        imageAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        imageAttr (TAttr StyleTag (Style x))   = X.thestyle x
        imageAttr (TAttr AltTag (Alt a))       = X.alt a
        imageAttr (TAttr SrcTag (Src a))       = X.src a
        imageAttr (TAttr WidthTag (Width w))   = X.width $ show w
        imageAttr (TAttr HeightTag (Height h)) = X.height $ show h

{-
renderTDocHtml (TLeaf tag content) = f tag
  where f :: Tag nodeTag -> Html
        f ContentTag = toHtml content

renderTDocHtml (TConcat tag child1 child2) = f tag
  where f :: Tag nodeTag -> Html
        f RootTag = renderTDocHtml child1 +++ renderTDocHtml child2
        f _       = error "impossible"
-}

ex :: IO ()
ex = putStr
     $ X.prettyHtml
     $ toHtml
     $ root
        (preambule ^<< title << FrQ "t")
        $ document $ toChildren $ do
            section (FrQ "s1") << do
              subsection (FrQ "ss1") << do
                paragraph << FrQ "p1"
                ulist << do
                  item << paragraph << FrQ "a"
                  item << paragraph << do
                    FrQ "b"
                paragraph << FrQ "p1"
            section (FrQ "s2") << do
              subsection (FrQ "ss2") << do
                paragraph << do
                  FrQ "p2"
                paragraph << map FrQ ["p3a", "p3b"]
                paragraph << leaf $ FrQ "p4"
            section (FrQ "s3") << ()
            section (FrQ "s4") << subsection (FrQ "ss4") << paragraph << FrQ "p5"

--end
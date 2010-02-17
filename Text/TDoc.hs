{-# LANGUAGE Rank2Types, TypeFamilies, ScopedTypeVariables, GADTs, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
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
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy.Char8  as L8
import qualified Data.ByteString       as Strict  (ByteString)
import qualified Data.ByteString.Lazy  as Lazy    (ByteString)

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
data Table
data Row
data Col
data HCol
data Div a
data Label
data Input
data Form
data Leaf
newtype Style = Style { fromStyle :: String } -- put something more typeful
newtype Url = Url { fromUrl :: String }
newtype Alt = Alt { fromAlt :: String }
newtype Src = Src { fromSrc :: String }
newtype ClassAttr = ClassAttr { fromClassAttr :: String }
newtype Name = Name { fromName :: String }
newtype Value = Value { fromValue :: String }
newtype Action = Action { fromAction :: String }
data InputType = TEXT
               | NUMBER -- check me
               | SUBMIT
               | RawInputType String
data FormMethod = GET
                | POST
                | RawFormMethod String

data Size = Px Int
          | Cm Int
          | Em Int
newtype Width = Width { fromWidth :: Size }
newtype Height = Height { fromHeight :: Size }

instance Show Size where
  show (Px x) = show x ++ "px"
  show (Cm x) = show x ++ "cm"
  show (Em x) = show x ++ "em"

toPixels :: Size -> Int
toPixels (Px x) = x
toPixels _ = error "toPixels: wrong unit"

instance Show FormMethod where
  show POST               = "post"
  show GET                = "get"
  show (RawFormMethod s)  = s

instance Show InputType where
  show TEXT              = "text"
  show NUMBER            = "number"
  show SUBMIT            = "submit"
  show (RawInputType s)  = s

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
  HLinkTag      :: Url -> Tag HLink
  TitleTag      :: Tag Title
  ImageTag      :: Tag Image
  BrTag         :: Tag Br
  HrTag         :: Tag Hr
  RawHtmlTag    :: Html -> Tag a
  TableTag      :: Tag Table
  RowTag        :: Tag Row
  ColTag        :: Tag Col
  HColTag       :: Tag HCol
  DivTag        :: Tag (Div a)
  FormTag       :: Tag Form
  InputTag      :: Tag Input
  LabelTag      :: Tag Label

  StyleTag      :: Tag Style
  AltTag        :: Tag Alt
  SrcTag        :: Tag Src
  WidthTag      :: Tag Width
  HeightTag     :: Tag Height
  ClassAttrTag  :: Tag ClassAttr
  InputTypeTag  :: Tag InputType
  NameTag       :: Tag Name
  ValueTag      :: Tag Value
  FormMethodTag :: Tag FormMethod
  ActionTag     :: Tag Action
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
instance IsNode Table
instance IsNode Row
instance IsNode Col
instance IsNode HCol
instance IsNode a => IsNode (Div a)
instance IsNode Label
instance IsNode Input
instance IsNode Form

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
instance IsBlock Label
instance IsBlock Form
instance IsBlock Input

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

-- instance IsNode a => Child Any a

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
instance Child Document Form
instance a ~ Document => Child Document (Div a)

instance Child Section Subsection
-- instance IsBlock a => Child Section a
instance Child Section Paragraph
instance a ~ Section => Child Section (Div a)
instance Child Section UList
instance Child Section Table
instance Child Section Hr
instance Child Section Form

instance IsBlock a => Child Subsection a

instance IsBlockOrInline a => Child Col a
instance IsBlockOrInline a => Child HCol a

instance IsInline a => Child Paragraph a
instance IsInline a => Child Span a
instance IsInline a => Child HLink a
instance IsInline a => Child Label a

instance IsBlockOrInline a => Child Item a

instance Child UList Item
instance Child Row Col
instance Child Row HCol
instance Child Table Row
instance Child Form Label
instance Child Form Input
instance Form ~ a => Child Form (Div a)

class (IsAttribute attr, IsNode node) => IsAttributeOf attr node

-- instance IsAttribute Url
instance IsAttribute Alt
instance IsAttribute Src
instance IsAttribute Height
instance IsAttribute Width
instance IsAttribute Style
instance IsAttribute ClassAttr
instance IsAttribute Name
instance IsAttribute Value
instance IsAttribute Action
instance IsAttribute InputType
instance IsAttribute FormMethod

instance IsNode a => IsAttributeOf ClassAttr a
instance IsNode n => IsAttributeOf Style n
instance IsAttributeOf Alt Image
instance IsAttributeOf Src Image
instance IsAttributeOf Height Image
instance IsAttributeOf Width Image
instance IsAttributeOf Name Input
instance IsAttributeOf Value Input
instance IsAttributeOf InputType Input
instance IsAttributeOf Action Form
instance IsAttributeOf FormMethod Form

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

class HasLeaves a where
  charToChildren :: Char -> [TChildOf a]
  lazyByteStringToChildren :: Lazy.ByteString -> [TChildOf a]
  strictByteStringToChildren :: Strict.ByteString -> [TChildOf a]

instance HasLeaves Title      where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves Paragraph  where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves Span       where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves HLink      where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves Label      where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves Col        where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString
instance HasLeaves HCol       where
  charToChildren              = toChildren . char
  lazyByteStringToChildren    = toChildren . lazyByteString
  strictByteStringToChildren  = toChildren . strictByteString

instance HasLeaves a => ToChildren Char               a where toChildren = charToChildren
instance HasLeaves a => ToChildren Lazy.ByteString    a where toChildren = lazyByteStringToChildren
instance HasLeaves a => ToChildren Strict.ByteString  a where toChildren = strictByteStringToChildren

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

form :: AttributesOf Form -> TDocMaker Form
form eta = tNode FormTag eta

input :: AttributesOf Input -> TDoc Input
input attrs = TNode InputTag attrs []

label :: AttributesOf Label -> TDocMaker Label
label eta = tNode LabelTag eta

div :: AttributesOf (Div a) -> TDocMaker (Div a)
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

rawHtml :: Html -> TDoc a
rawHtml h = TNode (RawHtmlTag h) [] []

char :: Char -> TDoc Leaf
char = rawHtml . toHtml

string :: String -> TDoc Leaf
string = rawHtml . toHtml

strictByteString :: Strict.ByteString -> TDoc Leaf
strictByteString = string . S8.unpack

lazyByteString :: Lazy.ByteString -> TDoc Leaf
lazyByteString = string . L8.unpack

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

value :: IsAttributeOf Value a => String -> AttributeOf a
value = TAttr ValueTag . Value

name :: IsAttributeOf Name a => String -> AttributeOf a
name = TAttr NameTag . Name

inputType :: IsAttributeOf InputType a => InputType -> AttributeOf a
inputType = TAttr InputTypeTag

formMethod :: IsAttributeOf FormMethod a => FormMethod -> AttributeOf a
formMethod = TAttr FormMethodTag

action :: IsAttributeOf Action a => String -> AttributeOf a
action = TAttr ActionTag . Action

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
        f SpanTag       = genSpan (lookupClassAttr attrs)
        f (HLinkTag url)= toHtml $ X.hotlink (fromUrl url) X.! map hlinkAttr attrs X.<< children
        f ImageTag      = assert (null children) $ X.image X.! map imageAttr attrs
        f BrTag         = assert (null children) $ X.br X.! map commonAttr attrs
        f HrTag         = assert (null children) $ X.hr X.! map commonAttr attrs
        f (RawHtmlTag h)= assert (null children) $ assert (null attrs) h
        f FormTag       = X.form X.! map formAttr attrs X.<< children
        f LabelTag      = X.label X.! map commonAttr attrs X.<< children
        f InputTag      = X.input X.! map inputAttr attrs
        f ClassAttrTag  = error "impossible"
        f AltTag        = error "impossible"
        f StyleTag      = error "impossible"
        f SrcTag        = error "impossible"
        f WidthTag      = error "impossible"
        f HeightTag     = error "impossible"
        f ActionTag     = error "impossible"
        f NameTag       = error "impossible"
        f ValueTag      = error "impossible"
        f FormMethodTag = error "impossible"
        f InputTypeTag  = error "impossible"

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

        inputAttr :: AttributeOf Input -> HtmlAttr
        inputAttr (TAttr ClassAttrTag (ClassAttr x))  = X.theclass x
        inputAttr (TAttr StyleTag (Style x))          = X.thestyle x
        inputAttr (TAttr InputTypeTag it)             = X.thetype . show $ it
        inputAttr (TAttr NameTag (Name n))            = X.name n
        inputAttr (TAttr ValueTag (Value n))          = X.value n
        inputAttr _                                   = error "inputAttr: bug"

        formAttr :: AttributeOf Form -> HtmlAttr
        formAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        formAttr (TAttr StyleTag (Style x))   = X.thestyle x
        formAttr (TAttr FormMethodTag fm)     = X.method . show $ fm
        formAttr (TAttr ActionTag (Action a)) = X.action a
        formAttr _ = error "formAttr: bug"

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

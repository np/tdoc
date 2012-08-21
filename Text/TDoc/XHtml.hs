--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.XHtml
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, ScopedTypeVariables, GADTs,
             FlexibleContexts, TemplateHaskell, TypeOperators #-}
module Text.TDoc.XHtml where

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict (HTML, Html, HtmlAttr, toHtml)
import Control.Arrow (second)
import Control.Exception (assert)
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy.Char8  as L8
import Text.TDoc.Core
import Text.TDoc.Tags
import Text.TDoc.TH
import Text.TDoc.Attributes
import Text.TDoc.Tags.Form

type HtmlAttributeOf    = AttributeOf HtmlTag
type HtmlAttributesOf x = AttributesOf HtmlTag x
type HtmlDoc            = TDoc HtmlTag

data HtmlTag t where
  RootTag       :: HtmlTag Root
  PreambuleTag  :: HtmlTag Preambule
  DocumentTag   :: HtmlTag Document
  SectionTag    :: (a `IsChildOf` Span) => HtmlDoc a -> HtmlTag Section
  SubsectionTag :: (a `IsChildOf` Span) => HtmlDoc a -> HtmlTag Subsection
  UListTag      :: HtmlTag UList
  ItemTag       :: HtmlTag Item
  ParagraphTag  :: HtmlTag Paragraph
  SpanTag       :: HtmlTag Span
  AnchorTag     :: HtmlTag Anchor
  HLinkTag      :: Url -> HtmlTag HLink
  TitleTag      :: HtmlTag Title
  ImageTag      :: HtmlTag Image
  BrTag         :: HtmlTag Br
  HrTag         :: HtmlTag Hr
  RawHtmlTag    :: Html -> HtmlTag a
  TableTag      :: HtmlTag Table
  RowTag        :: HtmlTag Row
  ColTag        :: HtmlTag Col
  HColTag       :: HtmlTag HCol
  DivTag        :: HtmlTag (Div a)
  FormTag       :: HtmlTag Form
  InputTag      :: HtmlTag Input
  OptionTag     :: HtmlTag Option
  SelectTag     :: HtmlTag Select
  TextareaTag   :: HtmlTag Textarea
  LabelTag      :: HtmlTag Label
  StyleTag      :: HtmlTag Style
  IdentifierTag :: HtmlTag Identifier
  HrefTag       :: HtmlTag Href
  AltTag        :: HtmlTag Alt
  SrcTag        :: HtmlTag Src
  WidthTag      :: HtmlTag Width
  HeightTag     :: HtmlTag Height
  ClassAttrTag  :: HtmlTag ClassAttr
  InputTypeTag  :: HtmlTag InputType
  NameTag       :: HtmlTag Name
  ValueTag      :: HtmlTag Value
  FormMethodTag :: HtmlTag FormMethod
  ActionTag     :: HtmlTag Action
  SelectedTag   :: HtmlTag Selected
  MultipleTag   :: HtmlTag Multiple
  SizeTag       :: HtmlTag Size
  RowsTag       :: HtmlTag Rows
  ColsTag       :: HtmlTag Cols

instance LeafTags HtmlTag where
  charTag              = RawHtmlTag . toHtml
  stringTag            = RawHtmlTag . toHtml
  strictByteStringTag  = RawHtmlTag . toHtml . S8.unpack
  lazyByteStringTag    = RawHtmlTag . toHtml . L8.unpack

-- instance ValueAttributeTag HtmlTag where valueTag = ValueTag
-- ...
-- instance ActionAttributeTag HtmlTag where actionTag = ActionTag
$(tagInstances ''HtmlTag [''Value, ''Action, ''FormMethod
                         ,''Selected, ''InputType, ''Multiple
                         ,''Form, ''Input, ''Option, ''Select
                         ,''Textarea, ''Label, ''Style, ''Src
                         ,''Height, ''Width, ''ClassAttr, ''Alt
                         ,''Name, ''Size, ''Rows, ''Cols, ''Span
                         ,''Anchor, ''Root, ''Preambule, ''Document
                         ,''UList, ''Item, ''Paragraph
                         ,''Title, ''Image, ''Br, ''Hr, ''Table
                         ,''Row, ''Col, ''HCol, ''Section, ''Subsection
                         ,''Div, ''HLink, ''Identifier, ''Href
                         ])

instance FormAttributeTags HtmlTag
instance FormTags HtmlTag
instance AttributeTags HtmlTag
instance Tags HtmlTag

rawHtml :: Html -> HtmlDoc a
rawHtml = tNullary . RawHtmlTag

rawHtml_ :: a -> Html -> HtmlDoc a
rawHtml_ _ = rawHtml

lookupClassAttr :: IsAttributeOf ClassAttr nodeTag => HtmlAttributesOf nodeTag -> Maybe (String, HtmlAttributesOf nodeTag)
lookupClassAttr (TAttr ClassAttrTag (ClassAttr t) : attrs) = Just (t, attrs)
lookupClassAttr (attr : attrs) = fmap (second (attr:)) $ lookupClassAttr attrs
lookupClassAttr [] = Nothing

renderTDocHtml :: forall nodeTag . IsNode nodeTag => HtmlDoc nodeTag -> Html
renderTDocHtml (TNode tag attrs children) = f tag
  where f :: IsNode nodeTag => HtmlTag nodeTag -> Html
        f RootTag       = toHtml children
        f PreambuleTag  = X.header   X.! commonAttrs "head"  attrs X.<< children
        f TitleTag      = X.thetitle X.! commonAttrs "title" attrs X.<< children
        f DocumentTag   = X.body     X.! commonAttrs "body"  attrs X.<< children
        f (SectionTag x)     = heading X.h1 x
        f (SubsectionTag x)  = heading X.h2 x
        f UListTag      = X.ulist    X.! commonAttrs "ul"     attrs X.<< children
        f ItemTag       = X.li       X.! commonAttrs "li"     attrs X.<< children
        f ParagraphTag  = X.p        X.! commonAttrs "p"      attrs X.<< children
        f DivTag        = X.thediv   X.! commonAttrs "div"   attrs X.<< children
        f TableTag      = X.table    X.! commonAttrs "table" attrs X.<< children
        f ColTag        = X.td       X.! commonAttrs "td" attrs X.<< children
        f HColTag       = X.th       X.! commonAttrs "th" attrs X.<< children
        f RowTag        = X.tr       X.! commonAttrs "tr" attrs X.<< children
        f SpanTag       = genSpan (lookupClassAttr attrs)
        f (HLinkTag url)= toHtml $ X.hotlink (fromUrl url) X.! map hlinkAttr attrs X.<< children
        f ImageTag      = assert (null children) $ X.image X.! map imageAttr         attrs
        f BrTag         = assert (null children) $ X.br    X.! commonAttrs "br" attrs
        f HrTag         = assert (null children) $ X.hr    X.! commonAttrs "hr" attrs
        f InputTag      = assert (null children) $ X.input X.! map inputAttr         attrs
        f (RawHtmlTag h)= assert (null children) $ assert (null attrs) h
        f FormTag       = X.form      X.! map formAttr             attrs X.<< children
        f LabelTag      = X.label     X.! commonAttrs "label" attrs X.<< children
        f SelectTag     = X.select    X.! map selectAttr           attrs X.<< children
        f TextareaTag   = X.textarea  X.! map textareaAttr         attrs X.<< children
        f OptionTag     = X.option    X.! map optionAttr           attrs X.<< children
        f AnchorTag     = X.anchor    X.! map anchorAttr           attrs X.<< children
        f ClassAttrTag  = error "impossible"
        f AltTag        = error "impossible"
        f StyleTag      = error "impossible"
        f IdentifierTag = error "impossible"
        f HrefTag       = error "impossible"
        f SrcTag        = error "impossible"
        f WidthTag      = error "impossible"
        f HeightTag     = error "impossible"
        f ActionTag     = error "impossible"
        f NameTag       = error "impossible"
        f ValueTag      = error "impossible"
        f FormMethodTag = error "impossible"
        f InputTypeTag  = error "impossible"
        f SelectedTag   = error "impossible"
        f MultipleTag   = error "impossible"
        f SizeTag       = error "impossible"
        f RowsTag       = error "impossible"
        f ColsTag       = error "impossible"

        heading :: (a `IsChildOf` Span) => (Html -> Html) -> HtmlDoc a -> Html
        heading hN child = hN {-X.! map (commonAttr attrs-} X.<< child X.+++ children

        genSpan :: nodeTag ~ Span => Maybe (String, HtmlAttributesOf nodeTag) -> Html
        genSpan (Just ("strong", attrs'))   = X.strong X.! commonAttrs "strong" attrs' X.<< children
        genSpan (Just ("italics", attrs'))  = X.italics X.! commonAttrs "i" attrs' X.<< children
        genSpan (Just ("tt", attrs'))       = X.tt X.! commonAttrs "tt" attrs' X.<< children
        genSpan (Just ("small", attrs'))    = X.small X.! commonAttrs "small" attrs' X.<< children
        genSpan (Just ("big", attrs'))      = X.big X.! commonAttrs "big" attrs' X.<< children
        genSpan (Just ("sub", attrs'))      = X.sub X.! commonAttrs "sub" attrs' X.<< children
        genSpan (Just ("sup", attrs'))      = X.sup X.! commonAttrs "sup" attrs' X.<< children
        genSpan (Just ("bold", attrs'))     = X.bold X.! commonAttrs "bold" attrs' X.<< children
        genSpan _  | null attrs = toHtml children
                   | otherwise  = X.thespan X.! commonAttrs "span" attrs X.<< children

        commonAttr :: IsNode a => String -> HtmlAttributeOf a -> HtmlAttr
        commonAttr _ (TAttr ClassAttrTag (ClassAttr x))   = X.theclass x
        commonAttr _ (TAttr StyleTag (Style s))           = X.thestyle s
        commonAttr _ (TAttr IdentifierTag (Identifier i)) = X.identifier i
        commonAttr nam _ = error $ "commonAttr: " ++ nam ++ ": bug"

        commonAttrs :: IsNode a => String -> [HtmlAttributeOf a] -> [HtmlAttr]
        commonAttrs = map . commonAttr

        hlinkAttr :: HtmlAttributeOf HLink -> HtmlAttr
        hlinkAttr = undefined

        inputAttr :: HtmlAttributeOf Input -> HtmlAttr
        inputAttr (TAttr InputTypeTag it)             = X.thetype . show $ it
        inputAttr (TAttr NameTag (Name n))            = X.name n
        inputAttr (TAttr ValueTag (Value n))          = X.value n
        inputAttr attr                                = commonAttr "input" attr

        formAttr :: HtmlAttributeOf Form -> HtmlAttr
        formAttr (TAttr FormMethodTag fm)     = X.method . show $ fm
        formAttr (TAttr ActionTag (Action a)) = X.action a
        formAttr attr = commonAttr "form" attr

        imageAttr :: HtmlAttributeOf Image -> HtmlAttr
        imageAttr (TAttr AltTag (Alt a))        =  X.alt a
        imageAttr (TAttr SrcTag (Src a))        =  X.src a
        imageAttr (TAttr WidthTag (Width w))    =  X.width . show . toPixels $ w
        imageAttr (TAttr HeightTag (Height h))  =  X.height . show . toPixels $ h
        imageAttr attr = commonAttr "img" attr

        selectAttr :: HtmlAttributeOf Select -> HtmlAttr
        selectAttr (TAttr MultipleTag  Multiple)  = X.multiple
        selectAttr (TAttr NameTag      (Name x))  = X.name x
        selectAttr (TAttr SizeTag      (Size x))  = X.size (show x)
        selectAttr attr = commonAttr "select" attr

        textareaAttr :: HtmlAttributeOf Textarea -> HtmlAttr
        textareaAttr (TAttr  NameTag  (Name x))  = X.name x
        textareaAttr (TAttr  RowsTag  (Rows x))  = X.rows (show x)
        textareaAttr (TAttr  ColsTag  (Cols x))  = X.cols (show x)
        textareaAttr attr = commonAttr "textarea" attr

        optionAttr :: HtmlAttributeOf Option -> HtmlAttr
        optionAttr (TAttr ValueTag      (Value n))      = X.value n
        optionAttr (TAttr SelectedTag   Selected)       = X.selected
        optionAttr attr = commonAttr "option" attr

        anchorAttr :: HtmlAttributeOf Anchor -> HtmlAttr
        anchorAttr (TAttr HrefTag (Href url)) = X.href url
        anchorAttr attr = commonAttr "a" attr

{-
TODO
commonAttr from http://www.html-5.com/attributes/

accesskey
class
contenteditable
contextmenu
draggable
dropzone
hidden
id
dir
lang
spellcheck
style
tabindex
title
Author-Defined data-* Attributes
on... Event Attributes
Microdata item... Attributes
XML Atttributes in HTML xmlns and xml:space

DONE
-}

instance (t ~ HtmlTag, IsNode a) => HTML (TDoc t a) where toHtml = renderTDocHtml

instance t ~ HtmlTag => HTML (ChildOf t fatherTag) where
  toHtml (Child x) = renderTDocHtml x

ex :: IO ()
ex = putStr
     $ X.prettyHtml
     $ toHtml
     $ root
        (preambule $ title "t")
        $ document $ do
            section "s1" <<
              subsection "ss1" << do
                para << "p1"
                ulist << do
                  item << para "a"
                  item << para << do
                    put "b"
                    put "c"
                para << "p1"
            section "s2" <<
              subsection "ss2" << do
                para << do
                  put "p2a"
                  put br
                  put "p2b"
                para << ["p3a", "p3b"]
                put hr
                para << string "p4"
                put $ para ["p5a", "p5b"]
            section "s3" << ()
            put hr
            section "s4" << subsection "ss4" << para << "p5"
            section "s5" << [hr,hr]

--end

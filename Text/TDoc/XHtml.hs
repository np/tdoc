{-# LANGUAGE TypeFamilies, ScopedTypeVariables, GADTs, FlexibleContexts #-}
module Text.TDoc.XHtml where

import qualified Text.XHtml.Strict as X
import Text.XHtml.Strict (HTML, Html, HtmlAttr, toHtml)
import Control.Arrow (second)
import Control.Exception (assert)
import qualified Data.ByteString.Char8       as S8
import qualified Data.ByteString.Lazy.Char8  as L8
import Text.TDoc.Core
import Text.TDoc.Tags
import Text.TDoc.Attributes
import Text.TDoc.Tags.Form

instance (t ~ HtmlTag, IsNode a) => HTML (TDoc t a) where toHtml = renderTDocHtml

instance t ~ HtmlTag => HTML (TChildOf t fatherTag) where
  toHtml (TChild x) = renderTDocHtml x

data HtmlTag t where
  RootTag       :: HtmlTag Root
  PreambuleTag  :: HtmlTag Preambule
  DocumentTag   :: HtmlTag Document
  SectionTag    :: Child Span a => HtmlDoc a -> HtmlTag Section
  SubsectionTag :: Child Span a => HtmlDoc a -> HtmlTag Subsection
  UListTag      :: HtmlTag UList
  ItemTag       :: HtmlTag Item
  ParagraphTag  :: HtmlTag Paragraph
  SpanTag       :: HtmlTag Span
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

instance ValueAttributeTag HtmlTag where
  valueTag             = ValueTag

instance FormAttributeTags HtmlTag where
  inputTypeTag         = InputTypeTag
  formMethodTag        = FormMethodTag
  actionTag            = ActionTag
  selectedTag          = SelectedTag
  multipleTag          = MultipleTag

instance FormTags HtmlTag where
  formTag              = FormTag
  inputTag             = InputTag
  optionTag            = OptionTag
  selectTag            = SelectTag
  textareaTag          = TextareaTag
  labelTag             = LabelTag

instance StyleAttrTag HtmlTag where
  styleTag             = StyleTag

instance SrcAttrTag HtmlTag where
  srcTag               = SrcTag

instance HeightAttrTag HtmlTag where
  heightTag            = HeightTag

instance WidthAttrTag HtmlTag where
  widthTag             = WidthTag

instance ClassAttrTag HtmlTag where
  classAttrTag         = ClassAttrTag

instance AttributeTags HtmlTag where
  altTag               = AltTag
  nameTag              = NameTag
  sizeTag              = SizeTag
  rowsTag              = RowsTag
  colsTag              = ColsTag

instance SpanTag HtmlTag where
  spanTag              = SpanTag

instance Tags HtmlTag where
  rootTag              = RootTag
  preambuleTag         = PreambuleTag
  documentTag          = DocumentTag
  sectionTag           = SectionTag
  subsectionTag        = SubsectionTag
  uListTag             = UListTag
  itemTag              = ItemTag
  paragraphTag         = ParagraphTag
  hLinkTag             = HLinkTag
  titleTag             = TitleTag
  imageTag             = ImageTag
  brTag                = BrTag
  hrTag                = HrTag
  tableTag             = TableTag
  rowTag               = RowTag
  colTag               = ColTag
  hColTag              = HColTag
  divTag               = DivTag

type HtmlAttributeOf = AttributeOf HtmlTag
type HtmlAttributesOf x = AttributesOf HtmlTag x
type HtmlDoc = TDoc HtmlTag

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
        f FormTag       = X.form      X.! map formAttr      attrs X.<< children
        f LabelTag      = X.label     X.! map commonAttr    attrs X.<< children
        f InputTag      = X.input     X.! map inputAttr     attrs
        f SelectTag     = X.select    X.! map selectAttr    attrs X.<< children
        f TextareaTag   = X.textarea  X.! map textareaAttr  attrs X.<< children
        f OptionTag     = X.option    X.! map optionAttr    attrs X.<< children
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
        f SelectedTag   = error "impossible"
        f MultipleTag   = error "impossible"
        f SizeTag       = error "impossible"
        f RowsTag       = error "impossible"
        f ColsTag       = error "impossible"

        heading :: Child Span a => (Html -> Html) -> HtmlDoc a -> Html
        heading hN child = hN {-X.! map commonAttr attrs-} X.<< child X.+++ children

        genSpan :: nodeTag ~ Span => Maybe (String, HtmlAttributesOf nodeTag) -> Html
        genSpan (Just ("strong", attrs')) = X.strong X.! map commonAttr attrs' X.<< children
        genSpan (Just ("italics", attrs')) = X.italics X.! map commonAttr attrs' X.<< children
        genSpan (Just ("tt", attrs')) = X.tt X.! map commonAttr attrs' X.<< children
        genSpan (Just ("small", attrs')) = X.small X.! map commonAttr attrs' X.<< children
        genSpan (Just ("big", attrs')) = X.big X.! map commonAttr attrs' X.<< children
        genSpan (Just ("sub", attrs')) = X.sub X.! map commonAttr attrs' X.<< children
        genSpan (Just ("sup", attrs')) = X.sup X.! map commonAttr attrs' X.<< children
        genSpan (Just ("bold", attrs')) = X.bold X.! map commonAttr attrs' X.<< children
        genSpan _  | null attrs = toHtml children
                   | otherwise  = X.thespan X.! map commonAttr attrs X.<< children

        commonAttr :: IsNode a => HtmlAttributeOf a -> HtmlAttr
        commonAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        commonAttr (TAttr StyleTag (Style s)) = X.thestyle s
        commonAttr _ = error "commonAttr: bug"

        hlinkAttr :: HtmlAttributeOf HLink -> HtmlAttr
        hlinkAttr = undefined

        inputAttr :: HtmlAttributeOf Input -> HtmlAttr
        inputAttr (TAttr ClassAttrTag (ClassAttr x))  = X.theclass x
        inputAttr (TAttr StyleTag (Style x))          = X.thestyle x
        inputAttr (TAttr InputTypeTag it)             = X.thetype . show $ it
        inputAttr (TAttr NameTag (Name n))            = X.name n
        inputAttr (TAttr ValueTag (Value n))          = X.value n
        inputAttr _                                   = error "inputAttr: bug"

        formAttr :: HtmlAttributeOf Form -> HtmlAttr
        formAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        formAttr (TAttr StyleTag (Style x))   = X.thestyle x
        formAttr (TAttr FormMethodTag fm)     = X.method . show $ fm
        formAttr (TAttr ActionTag (Action a)) = X.action a
        formAttr _ = error "formAttr: bug"

        imageAttr :: HtmlAttributeOf Image -> HtmlAttr
        imageAttr (TAttr ClassAttrTag (ClassAttr x)) = X.theclass x
        imageAttr (TAttr StyleTag (Style x))   = X.thestyle x
        imageAttr (TAttr AltTag (Alt a))       = X.alt a
        imageAttr (TAttr SrcTag (Src a))       = X.src a
        imageAttr (TAttr WidthTag (Width w))   = X.width . show . toPixels $ w
        imageAttr (TAttr HeightTag (Height h)) = X.height . show . toPixels $ h
        imageAttr _ = error "imageAttr: bug"

        selectAttr :: HtmlAttributeOf Select -> HtmlAttr
        selectAttr (TAttr ClassAttrTag  (ClassAttr x))  = X.theclass x
        selectAttr (TAttr StyleTag      (Style x))      = X.thestyle x
        selectAttr (TAttr MultipleTag   Multiple)       = X.multiple
        selectAttr (TAttr NameTag       (Name x))       = X.name x
        selectAttr (TAttr SizeTag       (Size x))       = X.size (show x)
        selectAttr _ = error "selectAttr: bug"

        textareaAttr :: HtmlAttributeOf Textarea -> HtmlAttr
        textareaAttr (TAttr ClassAttrTag  (ClassAttr x))  = X.theclass x
        textareaAttr (TAttr StyleTag      (Style x))      = X.thestyle x
        textareaAttr (TAttr NameTag       (Name x))       = X.name x
        textareaAttr (TAttr RowsTag       (Rows x))       = X.rows (show x)
        textareaAttr (TAttr ColsTag       (Cols x))       = X.cols (show x)
        textareaAttr _ = error "textareaAttr: bug"

        optionAttr :: HtmlAttributeOf Option -> HtmlAttr
        optionAttr (TAttr ClassAttrTag  (ClassAttr x))  = X.theclass x
        optionAttr (TAttr StyleTag      (Style x))      = X.thestyle x
        optionAttr (TAttr ValueTag      (Value n))      = X.value n
        optionAttr (TAttr SelectedTag   Selected)       = X.selected
        optionAttr _ = error "optionAttr: bug"

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

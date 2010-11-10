{-# LANGUAGE TypeFamilies, EmptyDataDecls,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Text.TDoc.Tags.Form where

import Text.TDoc.Core
import Text.TDoc.Attributes
import Text.TDoc.Tags

newtype  Action = Action { fromAction :: String }
instance IsAttribute Action

data     FormMethod = GET
                    | POST
                    | RawFormMethod String
instance IsAttribute FormMethod
instance Show FormMethod where
  show POST               = "post"
  show GET                = "get"
  show (RawFormMethod s)  = s

data     Selected = Selected
instance IsAttribute Selected

newtype  Value = Value { fromValue :: String }
instance IsAttribute Value

instance IsAttribute Multiple
data     Multiple = Multiple

data     Label
instance IsNode Label
instance IsBlock Label
instance IsInline a => Child Label a

data InputType
  = TEXT
  | PASSWORD
  | CHECKBOX
  | RADIO
  | SUBMIT
  | RESET
  | FILE
  | IMAGE
  | BUTTON
  | HIDDEN
  deriving (Eq, Ord, Enum)
instance IsAttribute InputType
instance Show InputType where
  show TEXT      = "text"
  show PASSWORD  = "password"
  show CHECKBOX  = "checkbox"
  show RADIO     = "radio"
  show SUBMIT    = "submit"
  show RESET     = "reset"
  show FILE      = "file"
  show IMAGE     = "image"
  show BUTTON    = "button"
  show HIDDEN    = "hidden"

data     Input
instance IsNode Input
instance IsAttributeOf Name Input
instance IsAttributeOf Value Input
instance IsAttributeOf InputType Input

data     Select
instance IsNode Select
instance Child Select Option
instance IsAttributeOf Multiple Select
instance IsAttributeOf Name Select
instance IsAttributeOf Size Select

data     Option
instance IsNode Option
instance Child Option Leaf
instance IsAttributeOf Selected Option
instance IsAttributeOf Value Option
class    ValueAttributeTag t where valueTag :: t Value
value    :: (ValueAttributeTag t, IsAttributeOf Value a) => String -> AttributeOf t a
value    = TAttr valueTag . Value

data     Textarea
instance IsNode Textarea
instance Child Textarea Leaf
instance IsAttributeOf Rows Textarea
instance IsAttributeOf Cols Textarea
instance IsAttributeOf Name Textarea

data     Form
instance IsNode Form
instance IsBlock Form
instance Form ~ a => Child Form (Div a)
instance IsAttributeOf Action Form
instance IsAttributeOf FormMethod Form
instance Child Document Form
instance Child Section  Form
instance Child Form Select
instance Child Form Textarea
instance Child Form Input
instance Child Form Label

class ValueAttributeTag t => FormAttributeTags t where
  inputTypeTag         :: t InputType
  formMethodTag        :: t FormMethod
  actionTag            :: t Action
  selectedTag          :: t Selected
  multipleTag          :: t Multiple

class FormAttributeTags t => FormTags t where
  formTag              :: t Form
  inputTag             :: t Input
  optionTag            :: t Option
  selectTag            :: t Select
  textareaTag          :: t Textarea
  labelTag             :: t Label

form :: FormTags t => Star t Form
form = tStar formTag

input :: FormTags t => Nullary t Input
input = tNullary inputTag

option :: FormTags t => Star t Option
option = tStar optionTag

-- actually Plus Select would be a more precise type
select :: FormTags t => Star t Select
select = tStar selectTag

selectQ :: (LeafTags t, FormTags t) => AttributesOf t Select -> (String, String) -> [(String, String)] -> TDoc t Select
selectQ attrs (val0, children0) opts
  = select ! attrs $ (option ! [value val0, selected] $ children0) : map f opts
  where
    f (val, children) = option ! [value val] $ children

textarea :: (FormTags t, AttributeTags t) => Rows -> Cols -> Star t Textarea
textarea r c = tStar textareaTag ! [rows (fromRows r), cols (fromCols c)]

label :: FormTags t => Star t Label
label = tStar labelTag

inputType :: (FormAttributeTags t, IsAttributeOf InputType a) => InputType -> AttributeOf t a
inputType = TAttr inputTypeTag

formMethod :: (FormAttributeTags t, IsAttributeOf FormMethod a) => FormMethod -> AttributeOf t a
formMethod = TAttr formMethodTag

action :: (FormAttributeTags t, IsAttributeOf Action a) => String -> AttributeOf t a
action = TAttr actionTag . Action

selected :: (FormAttributeTags t, IsAttributeOf Selected a) => AttributeOf t a
selected = TAttr selectedTag Selected

selectedB :: (FormAttributeTags t, IsAttributeOf Selected a) => Bool -> AttributesOf t a -> AttributesOf t a
selectedB True   = (TAttr selectedTag Selected:)
selectedB False  = id

selectedMS :: (FormAttributeTags t, IsAttributeOf Selected a) => Maybe Selected -> AttributesOf t a -> AttributesOf t a
selectedMS (Just Selected) = (TAttr selectedTag Selected:)
selectedMS Nothing         = id


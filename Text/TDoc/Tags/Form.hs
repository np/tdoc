--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.Tags.Form
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, EmptyDataDecls, TemplateHaskell,
             MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Text.TDoc.Tags.Form where

import Text.TDoc.Core
import Text.TDoc.TH
import Text.TDoc.Attributes
import Text.TDoc.Tags

newtype  Action = Action { fromAction :: String }
data     Selected = Selected
newtype  Value = Value { fromValue :: String }
data     Multiple = Multiple
data     FormMethod = GET
                    | POST
                    | RawFormMethod String

instance Show FormMethod where
  show POST               = "post"
  show GET                = "get"
  show (RawFormMethod s)  = s

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

--

$(attributes [''FormMethod, ''Action, ''Selected
             ,''Value, ''Multiple, ''InputType])

--

formMethod :: (FormMethodTag t, IsAttributeOf FormMethod a) => FormMethod -> AttributeOf t a
formMethod = TAttr formMethodTag

action :: (ActionTag t, IsAttributeOf Action a) => String -> AttributeOf t a
action = TAttr actionTag . Action

selected :: (SelectedTag t, IsAttributeOf Selected a) => AttributeOf t a
selected = TAttr selectedTag Selected

selectedB :: (SelectedTag t, IsAttributeOf Selected a) => Bool -> AttributesOf t a -> AttributesOf t a
selectedB True   = (TAttr selectedTag Selected:)
selectedB False  = id

selectedMS :: (SelectedTag t, IsAttributeOf Selected a) => Maybe Selected -> AttributesOf t a -> AttributesOf t a
selectedMS (Just Selected) = (TAttr selectedTag Selected:)
selectedMS Nothing         = id

value    :: (ValueTag t, IsAttributeOf Value a) => String -> AttributeOf t a
value    = TAttr valueTag . Value

inputType :: (InputTypeTag t, IsAttributeOf InputType a) => InputType -> AttributeOf t a
inputType = TAttr inputTypeTag

--

$(node "Label" [] [] [])
--instance IsBlock Label
instance IsInline a => IsChildOf a Label
label :: LabelTag t => Star t Label
label = tStar labelTag

$(node "Input" [] [''Name, ''Value, ''InputType] [])
input :: InputTag t => Nullary t Input
input = tNullary inputTag

$(node "Option" [] [''Selected, ''Value] [''Leaf])
option :: OptionTag t => Star t Option
option = tStar optionTag

$(node "Select" [] [''Multiple, ''Name, ''Size] [''Option])
-- actually Plus Select would be a more precise type
select :: SelectTag t => Star t Select
select = tStar selectTag

$(node "Textarea" [] [''Rows, ''Cols, ''Name] [''Leaf])
textarea :: (TextareaTag t, AttributeTags t) => Rows -> Cols -> Star t Textarea
textarea r c = tStar textareaTag ! [rows (fromRows r), cols (fromCols c)]

$(node "Form" [Block] [''Action, ''FormMethod] [''Select, ''Textarea, ''Input, ''Label])
$(nodeChildren ''Document [''Form])
instance Form ~ a => IsChildOf (Div a) Form
form :: FormTags t => Star t Form
form = tStar formTag

--

class (ActionTag t
      ,ValueTag t
      ,FormMethodTag t
      ,SelectedTag t
      ,InputTypeTag t
      ,MultipleTag t
      ) => FormAttributeTags t

class (FormAttributeTags t
      ,LabelTag t
      ,InputTag t
      ,OptionTag t
      ,FormTag t
      ,SelectTag t
      ,TextareaTag t
      ) => FormTags t

selectQ :: (LeafTags t, FormTags t) => AttributesOf t Select -> (String, String) -> [(String, String)] -> TDoc t Select
selectQ attrs (val0, children0) opts
  = select ! attrs $ (option ! [value val0, selected] $ children0) : map f opts
  where
    f (val, children) = option ! [value val] $ children

{-# LANGUAGE Rank2Types, ExistentialQuantification, MultiParamTypeClasses,
             TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Text.TDoc.Core where

import Data.Monoid
import Data.Functor.Identity
import Control.Monad.Trans.Writer

class IsNode node

class IsAttribute attr

class (IsNode father, IsNode child) => Child father child

class (IsAttribute attr, IsNode node) => IsAttributeOf attr node

type AttributesOf t tag = [AttributeOf t tag] -- AttributesMap nodeTag

data TDoc t tag =
  TNode { tTag       :: t tag
        , tAttrs     :: AttributesOf t tag
        , tChildren  :: [TChildOf t tag]
        }

data TChildOf t fatherTag =
  forall childTag. Child fatherTag childTag =>
    TChild (TDoc t childTag)

type PutM a = Writer [a] ()

class ToTDoc t a b where
  toTDoc :: a -> TDoc t b

instance (t1 ~ t2, a ~ b) => ToTDoc t1 (TDoc t2 a) b where
  toTDoc = id

class ToChildren t a father where
  toChildren :: a -> [TChildOf t father]

instance ToChildren t a b => ToChildren t [a] b where
  toChildren = concatMap toChildren

instance ToChildren t () b where
  toChildren () = []

instance (t1 ~ t2, a ~ b) => ToChildren t1 (TChildOf t2 a) b where
  toChildren = (:[])

instance (t1 ~ t2, Child b a) => ToChildren t1 (TDoc t2 a) b where
  toChildren = (:[]) . TChild

-- TODO: try to generalize this...
instance (Monad m, ToChildren t [a] b, w ~ (), m ~ Identity) => ToChildren t (WriterT [a] m w) b where
  toChildren = toChildren . execWriter

type Star t node
  = forall children. ToChildren t children node =>
      children -> TDoc t node
type Nullary t node
  = TDoc t node
type Unary t node
  = forall child. Child node child =>
      TDoc t child -> TDoc t node
type Plus t node
  = forall children child. (Child node child, ToChildren t children node) =>
      TDoc t child -> children -> TDoc t node

data AttributeOf t nodeTag =
  forall attrTag. (attrTag `IsAttributeOf` nodeTag) =>
    TAttr (t attrTag) attrTag

-- try to use this
-- newtype AttributesMap nodeTag = AttrMap { getAttrMap :: Map AnyTag (AttributeOf nodeTag) }

infixl 8 !

class AddAttrs t a b where
  (!) :: a -> AttributesOf t b -> a

instance (t1 ~ t2, a ~ b) => AddAttrs t1 (TDoc t2 a) b where
  (TNode tag attrs children) ! attrs' = TNode tag (attrs++attrs') children

instance AddAttrs t b c => AddAttrs t (a -> b) c where
  (f ! attrs) x = f x ! attrs

(<>) :: Monoid m => m -> m -> m
(<>) = mappend


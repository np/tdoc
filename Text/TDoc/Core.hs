{-# LANGUAGE Rank2Types, ExistentialQuantification, MultiParamTypeClasses,
             TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Text.TDoc.Core where

import Data.Monoid
import Data.Functor.Identity
import Control.Monad.Trans.Writer

class IsNode node

class IsAttribute attr

class (IsNode father, IsNode child) => child `IsChildOf` father

class (IsAttribute attr, IsNode node) => IsAttributeOf attr node

class IsNode a => IsBlockOrInline a
class IsBlockOrInline a => IsInline a
class IsBlockOrInline a => IsBlock a

data AttributeOf t node =
  forall attr. (attr `IsAttributeOf` node) =>
    TAttr (t attr) attr

type AttributesOf t node = [AttributeOf t node]

data TDoc t tag =
  TNode { tTag       :: t tag
        , tAttrs     :: AttributesOf t tag
        , tChildren  :: [ChildOf t tag]
        }

data ChildOf t father =
  forall child. (child `IsChildOf` father) => Child (TDoc t child)

-- ToChildren instances should only dispatch on the first argument.
-- So the to others must be variables, however you can equate
-- them in the premises.
class ToChildren a t father where
  toChildren :: a -> [ChildOf t father]

instance ToChildren a t b => ToChildren [a] t b where
  toChildren = concatMap toChildren

instance ToChildren () t b where
  toChildren () = []

instance (ToChildren a t n, ToChildren b t n) => ToChildren (a,b) t n where
  toChildren (x, y) = toChildren x ++ toChildren y

instance (ToChildren a t n
         ,ToChildren b t n
         ,ToChildren c t n
         ) => ToChildren (a,b,c) t n where
  toChildren (x, y, z) = toChildren x ++ toChildren y ++ toChildren z

instance (t1 ~ t2, a ~ b) => ToChildren (ChildOf t1 a) t2 b where
  toChildren = (:[])

instance (t1 ~ t2, a `IsChildOf` b) => ToChildren (TDoc t1 a) t2 b where
  toChildren = toChildren . Child

instance ToChildren a t b => ToChildren (Identity a) t b where
  toChildren = toChildren . runIdentity

instance (Monad m, ToChildren (m w) t b, a ~ ()) => ToChildren (WriterT w m a) t b where
  toChildren = toChildren . execWriterT

-- Like ToChildren, only dispatch on first.
class ToTDoc a t b where
  toTDoc :: a -> TDoc t b

instance (t1 ~ t2, a ~ b) => ToTDoc (TDoc t1 a) t2 b where
  toTDoc = id

instance ToTDoc a t b => ToTDoc (Identity a) t b where
  toTDoc = toTDoc . runIdentity

instance (Monad m, ToTDoc (m w) t b, a ~ ()) => ToTDoc (WriterT w m a) t b where
  toTDoc = toTDoc . execWriterT

infixl 8 !

-- Like ToChildren, only dispatch on first.
class AddAttrs a t b where
  (!) :: a -> AttributesOf t b -> a

instance (t1 ~ t2, a ~ b) => AddAttrs (TDoc t1 a) t2 b where
  (TNode tag attrs children) ! attrs' = TNode tag (attrs++attrs') children

instance AddAttrs b t c => AddAttrs (a -> b) t c where
  (f ! attrs) x = f x ! attrs

-- Like ToChildren, only dispatch on first.
class FromTDoc a t tag where
  fromTDoc :: TDoc t tag -> a

instance (t1 ~ t2, a ~ b) => FromTDoc (TDoc t1 a) t2 b where
  fromTDoc = id

instance (t1 ~ t2, b `IsChildOf` a) => FromTDoc (ChildOf t1 a) t2 b where
  fromTDoc = Child

instance FromTDoc a t tag => FromTDoc [a] t tag where
  fromTDoc = (:[]) . fromTDoc

instance FromTDoc a t tag => FromTDoc (Identity a) t tag where
  fromTDoc = Identity . fromTDoc

instance (Monad m, FromTDoc w t tag, Monoid w, a ~ ()) => FromTDoc (WriterT w m a) t tag where
  fromTDoc = tell . fromTDoc

type PutM a = Writer [a] ()

type Star t node
  = forall children. ToChildren children t node =>
      children -> TDoc t node

type Nullary t node
  = TDoc t node

type Unary t node
  = forall child. (child `IsChildOf` node) =>
      TDoc t child -> TDoc t node

type Plus t node
  = forall children child. (child `IsChildOf` node, ToChildren children t node) =>
      TDoc t child -> children -> TDoc t node

infixr 7 <<
infixr 2 +++

(+++) :: (ToChildren a t tag, ToChildren b t tag) => a -> b -> [ChildOf t tag]
a +++ b = toChildren a ++ toChildren b

-- | This operator is an infix sugar for 'put'
-- @paragraph << do ...@ is equal to @put $ paragraph $ do ...@.
(<<) :: (a `IsChildOf` b) => (c -> TDoc t a) -> c -> PutM (ChildOf t b)
(<<) f = put . f

put :: ToChildren children t father => children -> PutM (ChildOf t father)
put = tell . toChildren

tStar :: t a -> Star t a
tStar tag = TNode tag [] . toChildren

tNullary :: t a -> Nullary t a
tNullary tag = TNode tag [] []

tUnary :: t a -> Unary t a
tUnary tag = tStar tag . toChildren

tPlus :: t a -> Plus t a
tPlus tag first rest = tStar tag (Child first : toChildren rest)

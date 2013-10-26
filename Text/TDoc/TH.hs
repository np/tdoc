--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.TH
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
module Text.TDoc.TH
  (node
  ,nodeChildren
  ,nodeAttributes
  ,attribute
  ,attributes
  ,tagInstances
  ,tagInstance
  ,NodeOpt(..)
  ,NodeOpts)
where

import Data.Char (toLower)
import Text.TDoc.Core (IsNode, IsChildOf, IsAttributeOf, IsAttribute
                      ,IsInline, IsBlock, IsBlockOrInline)
import Language.Haskell.TH (Name,mkName,nameBase,Pat(..),Dec(..)
                           ,Exp(..),Type(..),TyVarBndr(..),Body(..),Q)

data NodeOpt = NoTag | Inline | Block | BlockOrInline
  deriving (Eq)
type NodeOpts = [NodeOpt]

mkIs :: Name -> Name -> Dec
mkIs cl ty = InstanceD [] (ConT cl `AppT` ConT ty) []

node :: String -> NodeOpts -> [Name] -> [Name] -> Q [Dec]
node nod opts attrs children
  = return . concat
    $ [ [ DataD [] nodeNm [] [] []
        , mkIs ''IsNode nodeNm
        ]
      , [ mkIs ''IsInline nodeNm | Inline `elem` opts ]
      , [ mkIs ''IsBlock  nodeNm | Block `elem` opts  ]
      , [ mkIs ''IsBlockOrInline nodeNm | BlockOrInline `elem` opts
                                          || Block `elem` opts
                                          || Inline `elem` opts ]
      , [ mkTagClass nodeNm | NoTag `notElem` opts ]
      , map (`mkIsAttributeOf` nodeNm) attrs
      , map (`mkIsChildOf` nodeNm) children
      ]
  where nodeNm = mkName nod

lowerFirst :: String -> String
lowerFirst []     = error "Text.TDoc.TH.lowerFirst: []"
lowerFirst (x:xs) = toLower x : xs

mkTagName :: String -> Name
mkTagName x = mkName $ x ++ "Tag"

mkTagClass :: Name -> Dec
mkTagClass nm = ClassD [] nmTagTy [PlainTV t] []
                       [SigD nmTagFun (VarT t `AppT` ConT nm)]
  where nmBase   = nameBase  nm
        nmTagTy  = mkTagName nmBase
        nmTagFun = mkTagName $ lowerFirst nmBase
        t        = mkName "t"

attribute :: Name -> Q [Dec]
attribute attr = return
                   [ InstanceD [] (ConT ''IsAttribute `AppT` ConT attr) []
                   , mkTagClass attr
                   ]

attributes :: [Name] -> Q [Dec]
attributes = fmap concat . mapM attribute

tagInstances :: Name -> [Name] -> Q [Dec]
tagInstances tagTy = return . map (tagInstance tagTy)

tagInstance :: Name -> Name -> Dec
tagInstance tagTy nm =
  InstanceD [] (ConT nmTagCon `AppT` ConT tagTy)
            [ValD (VarP nmTagFun) (NormalB (ConE nmTagCon)) []]
  where
    nmBase   = nameBase nm
    nmTagFun = mkTagName . lowerFirst $ nmBase
    nmTagCon = mkTagName nmBase

mkIsChildOf :: Name -> Name -> Dec
mkIsChildOf child nod = InstanceD [] (ConT ''IsChildOf `AppT` ConT child `AppT` ConT nod) []

mkIsAttributeOf :: Name -> Name -> Dec
mkIsAttributeOf attr nod = InstanceD [] (ConT ''IsAttributeOf `AppT` ConT attr `AppT` ConT nod) []

nodeChildren :: Name -> [Name] -> Q [Dec]
nodeChildren nod = return . map (`mkIsChildOf` nod)

nodeAttributes :: Name -> [Name] -> Q [Dec]
nodeAttributes nod = return . map (`mkIsAttributeOf` nod)

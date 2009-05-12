{-# LANGUAGE TemplateHaskell #-}
module Text.TDoc.QQ (expandingQQ, e) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.TDoc (spanDoc, toChildren)
import Control.Arrow
import Data.Char (isSpace)

expandingQQExpr :: String -> TH.ExpQ
expandingQQExpr = TH.listE . go "" . stripIndents
  where
    go cur ('{':xs) = chunk cur $ antiq ys $ go "" zs
                        where (ys, zs) = cut (0::Int) xs
    go cur (x:xs)   = go (x:cur) xs
    go cur []       = chunk cur []

    chunk x xs | null x    = xs
               | otherwise = mkToChildren (TH.stringE (reverse x)) : xs

    antiq x xs | null x    = xs
               | otherwise = mkToChildren (TH.varE (TH.mkName x)) : xs

    mkToChildren = (TH.varE 'toChildren `TH.appE`)

    cut lvl ('{':xs) = first ('{':) $ cut (lvl+1) xs
    cut 0   ('}':xs) = ([], xs)
    cut lvl ('}':xs) = first ('}':) $ cut (lvl-1) xs
    cut lvl (x:xs)   = first (x:)   $ cut lvl xs
    cut _   []       = error "missing '}', unexpected end of quotation"

stripIndents :: String -> String
stripIndents = go
  where go (x:xs) | isSpace x = ' ' : go (dropWhile isSpace xs)
                  | otherwise = x:go xs
        go                 "" = ""

e, expandingQQ :: QuasiQuoter
expandingQQ = QuasiQuoter (\str->TH.varE 'spanDoc `TH.appE` expandingQQExpr str)
                          (error "ExpandingQQ: not available in patterns")
e = expandingQQ

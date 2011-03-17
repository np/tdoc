--------------------------------------------------------------------
-- !
-- Module     : Text.TDoc.QQ
-- Copyright  : (c) Nicolas Pouillard 2009-2011
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
--
--------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Text.TDoc.QQ (
    -- * frquotes support
    frQQ, frTop, frAntiq) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.TDoc (spanDoc, Star, Span, SpanTag(..), ToChildren(..), ChildOf(..))
import Data.Char (isSpace)
import Data.Monoid

frTop :: SpanTag t => Star t Span
frTop = spanDoc

frAntiq :: ToChildren a t father => a -> [ChildOf t father] 
frAntiq = toChildren

expandingQQExpr :: String -> TH.ExpQ
expandingQQExpr = chunk . stripIndents
  where
    chunk x | null x    = TH.varE 'mempty
            | otherwise = TH.varE 'toChildren `TH.appE` TH.stringE x

stripIndents :: String -> String
stripIndents = go
  where go (x:xs) | isSpace x = ' ' : go (dropWhile isSpace xs)
                  | otherwise = x:go xs
        go                 "" = ""

quasiQuoter :: String -> QuasiQuoter
quasiQuoter qqName =
  QuasiQuoter (err "expressions") (err "patterns")
-- if GHC7
              (err "types") (err "declarations")
-- endif
  where err kind _ = error $ qqName ++ ": not available in " ++ kind

frQQ :: QuasiQuoter
frQQ = (quasiQuoter "Text.TDoc.QQ.frQQ"){quoteExp = expandingQQExpr }

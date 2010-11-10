{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Text.TDoc.QQ (frQQ, frTop, frAntiq) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.TDoc (spanDoc, Star, Span, Tag(..), ToChildren(..), TChildOf(..))
import Data.Char (isSpace)
import Data.Monoid

frTop :: Tag t => Star t Span
frTop = spanDoc

frAntiq :: ToChildren t a father => a -> [TChildOf t father] 
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

frQQ :: QuasiQuoter
frQQ = QuasiQuoter expandingQQExpr
                   (error "Text.TDoc.QQ.frQQ: not available in patterns")

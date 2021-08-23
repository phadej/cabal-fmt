-- |
-- License: GPL-3.0-or-later
-- Copyright: Leo Zhang
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.GhcOptions (ghcOptionsF) where

import qualified Distribution.FieldGrammar  as C
import qualified Distribution.Parsec        as C
import qualified Distribution.Pretty        as C
import qualified Text.PrettyPrint           as PP

import CabalFmt.Fields
import CabalFmt.Prelude

import Data.List (sort)

ghcOptionsF :: FieldDescrs () ()
ghcOptionsF = singletonF "ghc-options" pretty parse

parse :: C.CabalParsing m => m [String]
parse = unpack' (C.alaList' C.NoCommaFSep C.Token') <$> C.parsec

pretty :: [String] -> PP.Doc
pretty = PP.vcat . map C.showToken . sort

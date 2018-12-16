{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.Extensions (
    otherExtensionsF,
    defaultExtensionsF,
    ) where

import Data.List                   (sortOn)
import Distribution.Compat.Newtype

import qualified Distribution.Parsec          as C
import qualified Distribution.Parsec.Newtypes as C
import qualified Distribution.Pretty          as C
import qualified Language.Haskell.Extension   as C
import qualified Text.PrettyPrint             as PP

import CabalFmt.Fields

otherExtensionsF :: FieldDescrs () ()
otherExtensionsF = singletonF "other-extensions" pretty parse

defaultExtensionsF :: FieldDescrs () ()
defaultExtensionsF = singletonF "other-extensions" pretty parse

parse :: C.CabalParsing m => m [C.Extension]
parse = unpack' (C.alaList' C.FSep C.MQuoted) <$> C.parsec

pretty :: [C.Extension] -> PP.Doc
pretty = PP.vcat . map C.pretty . sortOn show

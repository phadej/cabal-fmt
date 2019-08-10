-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.TestedWith (
    testedWithF,
    ) where

import Distribution.Compat.Newtype

import qualified Distribution.Parsec                as C
import qualified Distribution.Compiler                as C
import qualified Distribution.Parsec.Newtypes       as C
import qualified Distribution.Pretty                as C
import qualified Distribution.Types.VersionRange    as C
import qualified Text.PrettyPrint                   as PP
import qualified Data.Map.Strict as Map

import CabalFmt.Fields

testedWithF :: FieldDescrs () ()
testedWithF = singletonF "tested-with" pretty parse where
    parse :: C.CabalParsing m => m [(C.CompilerFlavor, C.VersionRange)]
    parse = unpack' (C.alaList' C.FSep C.TestedWith) <$> C.parsec

    pretty :: [(C.CompilerFlavor, C.VersionRange)] -> PP.Doc
    pretty tw0 = PP.fsep $ PP.punctuate PP.comma
        [ prettyC c PP.<+> C.pretty vr
        | (c, vr) <- Map.toList tw1
        ]
      where
        tw1 :: Map.Map C.CompilerFlavor C.VersionRange
        tw1 = Map.fromListWith C.unionVersionRanges tw0

        prettyC C.GHC   = PP.text "GHC"
        prettyC C.GHCJS = PP.text "GHCJS"
        prettyC c       = C.pretty c

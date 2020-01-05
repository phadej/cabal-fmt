-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.TestedWith (
    testedWithF,
    ) where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Compiler         as C
import qualified Distribution.Parsec           as C
import qualified Distribution.Parsec.Newtypes  as C
import qualified Distribution.Pretty           as C
import qualified Distribution.Version          as C
import qualified Text.PrettyPrint              as PP

import CabalFmt.Prelude
import CabalFmt.Fields
import CabalFmt.Options

testedWithF :: Options -> FieldDescrs () ()
testedWithF Options { optSpecVersion = ver } = singletonF "tested-with" pretty parse where
    parse :: C.CabalParsing m => m [(C.CompilerFlavor, C.VersionRange)]
    parse = unpack' (C.alaList' C.FSep C.TestedWith) <$> C.parsec

    pretty :: [(C.CompilerFlavor, C.VersionRange)] -> PP.Doc
    pretty tw0 = leadingComma ver
        [ prettyC c PP.<+> prettyVr vr
        | (c, vr) <- Map.toList tw1
        ]
      where
        tw1 :: Map.Map C.CompilerFlavor C.VersionRange
        tw1 = Map.fromListWith C.unionVersionRanges tw0

        -- TODO: Cabal 3.0 formatting!
        prettyVr vr = case isVersionSet vr of
            Just vs -> PP.sep $ mapTail (\doc -> PP.nest (-3) $ PP.text "||" PP.<+> doc) [ C.pretty (C.thisVersion v) | v <- Set.toList vs ]
            Nothing -> C.pretty vr

        prettyC C.GHC   = PP.text "GHC"
        prettyC C.GHCJS = PP.text "GHCJS"
        prettyC c       = C.pretty c

leadingComma :: C.CabalSpecVersion -> [PP.Doc] -> PP.Doc
leadingComma _ []  = PP.empty
leadingComma _ [x] = x
leadingComma v xs = PP.vcat $ zipWith comma (True : repeat False) xs where
    comma :: Bool -> PP.Doc -> PP.Doc
    comma isFirst doc
        | isFirst, v < C.CabalSpecV3_0 = PP.char ' ' PP.<+> doc
        | otherwise                    = PP.char ',' PP.<+> doc

isVersionSet :: C.VersionRange -> Maybe (Set C.Version)
isVersionSet vr = go Set.empty (C.asVersionIntervals vr) where
    go !acc [] = Just acc
    go acc ((C.LowerBound v C.InclusiveBound, C.UpperBound u C.InclusiveBound) : vis)
        | v == u    = go (Set.insert v acc) vis
    go _ _ = Nothing

mapTail :: (a -> a) -> [a] -> [a]
mapTail _ []     = []
mapTail f (x:xs) = x : map f xs

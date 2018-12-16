{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.Modules (
    otherModulesF,
    exposedModulesF,
    ) where

import Data.List                   (sortBy)
import Data.Function (on)
import Distribution.Compat.Newtype

import qualified Distribution.ModuleName      as C
import qualified Distribution.Parsec          as C
import qualified Distribution.Parsec.Newtypes as C
import qualified Distribution.Pretty          as C
import qualified Text.PrettyPrint             as PP

import CabalFmt.Fields

exposedModulesF :: FieldDescrs () ()
exposedModulesF = singletonF "exposed-modules" pretty parse

otherModulesF :: FieldDescrs () ()
otherModulesF = singletonF "other-modules" pretty parse

parse :: C.CabalParsing m => m [C.ModuleName]
parse = unpack' (C.alaList' C.VCat C.MQuoted) <$> C.parsec

pretty :: [C.ModuleName] -> PP.Doc
pretty = PP.vcat . map C.pretty . sortBy (cmp `on` C.prettyShow)
  where
    cmp a b = case dropCommonPrefix a b of
        ([], [])  -> EQ
        ([], _:_) -> LT
        (_:_, []) -> GT
        (a', b')  -> compare a' b'

dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
dropCommonPrefix [] [] = ([], [])
dropCommonPrefix [] ys = ([], ys)
dropCommonPrefix xs [] = (xs, [])
dropCommonPrefix xs@(x:xs') ys@(y:ys')
    | x == y    = dropCommonPrefix xs' ys'
    | otherwise = (xs, ys)

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.Modules (
    otherModulesF,
    exposedModulesF,
    ) where

import qualified Distribution.FieldGrammar as C
import qualified Distribution.ModuleName   as C
import qualified Distribution.Parsec       as C
import qualified Distribution.Pretty       as C
import qualified Text.PrettyPrint          as PP

import CabalFmt.Fields
import CabalFmt.Prelude

exposedModulesF :: FieldDescrs () ()
exposedModulesF = singletonF "exposed-modules" pretty parse

otherModulesF :: FieldDescrs () ()
otherModulesF = singletonF "other-modules" pretty parse

parse :: C.CabalParsing m => m [C.ModuleName]
parse = unpack' (C.alaList' C.VCat C.MQuoted) <$> C.parsec

pretty :: [C.ModuleName] -> PP.Doc
pretty
    = PP.vcat . map C.pretty
    . nub
    . sortBy (cmp `on` map strToLower . C.components)
  where
    cmp a b = case dropCommonPrefix a b of
        ([], [])  -> EQ
        ([], _:_) -> LT
        (_:_, []) -> GT
        (a', b')  -> compare a' b'

strToLower :: String -> String
strToLower = map toLower

dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
dropCommonPrefix [] [] = ([], [])
dropCommonPrefix [] ys = ([], ys)
dropCommonPrefix xs [] = (xs, [])
dropCommonPrefix xs@(x:xs') ys@(y:ys')
    | x == y    = dropCommonPrefix xs' ys'
    | otherwise = (xs, ys)

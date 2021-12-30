-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.SourceFiles (
    sourceFilesF,
    fileFields,
    ) where

#ifdef mingw32_HOST_OS
import System.FilePath.Windows (splitDirectories)
#else
import System.FilePath.Posix   (splitDirectories)
#endif

import qualified Distribution.FieldGrammar as C
import qualified Distribution.Fields       as C
import qualified Distribution.Parsec       as C
import qualified Distribution.Pretty       as C
import qualified Text.PrettyPrint          as PP

import CabalFmt.Fields
import CabalFmt.Prelude

sourceFilesF :: [FieldDescrs () ()]
sourceFilesF =
    [ singletonF f pretty parse
    | f <- fileFields
    ]

fileFields :: [C.FieldName]
fileFields =
    [ "extra-source-files"
    , "extra-doc-files"
    , "data-files"
    , "license-files"
    , "asm-sources"
    , "cmm-sources"
    , "c-sources"
    , "cxx-sources"
    , "js-sources"
    , "includes"
    , "install-includes"
    ]

parse :: C.CabalParsing m => m [FilePath]
parse = unpack' (C.alaList' C.VCat C.FilePathNT) <$> C.parsec

pretty :: [FilePath] -> PP.Doc
pretty
    = PP.vcat . map C.showFilePath
    . nub
    . sortBy (cmp `on` map strToLower . splitDirectories)
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

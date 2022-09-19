-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module CabalFmt.Fields.BuildDepends (
    buildDependsF,
    setupDependsF,
    buildToolDependsF,
) where

import Data.List (dropWhileEnd)

import qualified Distribution.CabalSpecVersion    as C
import qualified Distribution.FieldGrammar        as C
import qualified Distribution.Parsec              as C
import qualified Distribution.Pretty              as C
import qualified Distribution.Types.Dependency    as C
import qualified Distribution.Types.DependencyMap as C
import qualified Distribution.Types.ExeDependency as C
import qualified Distribution.Types.VersionRange  as C
import qualified Text.PrettyPrint                 as PP

import CabalFmt.Fields
import CabalFmt.Options
import CabalFmt.Prelude
import VersionInterval  (normaliseVersionRange, ConversionProblem (..))

setupDependsF :: Options -> FieldDescrs () ()
setupDependsF opts = singletonF "setup-depends" (pretty opts) parse

buildDependsF :: Options -> FieldDescrs () ()
buildDependsF opts = singletonF "build-depends" (pretty opts) parse

buildToolDependsF :: Options -> FieldDescrs () ()
buildToolDependsF opts = singletonF "build-tool-depends" (prettyExe opts) parseExe

parse :: C.CabalParsing m => m [C.Dependency]
parse = unpack' (C.alaList C.CommaVCat) <$> C.parsec

parseExe :: C.CabalParsing m => m [C.ExeDependency]
parseExe = unpack' (C.alaList C.CommaVCat) <$> C.parsec

normaliseVersionRange' :: C.VersionRange -> C.VersionRange
normaliseVersionRange' vr = either fromConversionProblem id (normaliseVersionRange vr) where
    fromConversionProblem :: ConversionProblem -> C.VersionRange
    fromConversionProblem IntervalsEmpty         = C.noVersion
    fromConversionProblem OtherConversionProblem = vr

pretty :: Options -> [C.Dependency] -> PP.Doc
pretty opts deps = case deps of
    []    -> PP.empty
    [dep] -> C.pretty (C.depPkgName dep) PP.<+> prettyVR vr'
      where
        vr' = normaliseVersionRange' (C.depVerRange dep)

        prettyVR vr | vr == C.anyVersion = PP.empty
                    | vr == C.noVersion  = PP.text "<0"
                    | otherwise          = C.pretty vr

    _ -> prettyMany opts deps'
      where
        deps' :: [(String, C.VersionRange)]
        deps' = sortOn (map toLower . fst)
              $ map (prettyDepNoVersion &&& C.depVerRange)
              $ C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

        prettyDepNoVersion :: C.Dependency -> String
        prettyDepNoVersion (C.Dependency pkg _ libs) =
          C.prettyShow (C.Dependency pkg C.anyVersion libs)


prettyExe :: Options -> [C.ExeDependency] -> PP.Doc
prettyExe opts deps = case deps of
    []    -> PP.empty
    [dep] -> PP.text (exeDepExeName dep) PP.<+> prettyVR vr'
      where
        vr' = normaliseVersionRange' (exeDepVerRange dep)

        prettyVR vr | vr == C.anyVersion = PP.empty
                    | vr == C.noVersion  = PP.text "<0"
                    | otherwise          = C.pretty vr

    _ -> prettyMany opts deps'
      where
        deps' :: [(String, C.VersionRange)]
        deps' = sortOn (map toLower . fst)
              $ map (exeDepExeName &&& exeDepVerRange)
              -- C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

exeDepExeName :: C.ExeDependency -> String
exeDepExeName (C.ExeDependency name exe _) =
    C.prettyShow name ++ ":" ++ C.prettyShow exe

exeDepVerRange :: C.ExeDependency -> C.VersionRange
exeDepVerRange (C.ExeDependency _ _ vr) = vr

prettyMany :: Options -> [(String, C.VersionRange)] -> PP.Doc
prettyMany Options { optSpecVersion = v, optTabular = tab } deps'
    = PP.vcat
    $ map PP.text
    $ tbl
    $ zipWith cols (True : repeat False) deps'
  where
    cols :: Bool -> (String, C.VersionRange) -> [String]
    cols isFirst (name, vr)
        | full vr'  = [comma, name]
        | otherwise = comma : name : "" : words (C.prettyShow vr')
      where
        vr' = normaliseVersionRange' vr

        comma | isFirst, v < C.CabalSpecV2_2 = " "
              | otherwise                    = ","

    full :: C.VersionRange -> Bool
    full vr = vr == C.anyVersion

    tbl :: [[String]] -> [String]
    tbl = if tab then table else map (concatSpaces . unwords)

-- returns rows.
table :: [[String]] -> [String]
table cells = map strip rows
  where
    cols      :: Int
    rowWidths :: [Int]
    rows      :: [String]

    (cols, rowWidths, rows) = foldr go (0, repeat 0, []) cells

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go xs (c, w, yss) =
        ( max c (length xs)
        , zipWith max w (map length xs ++ repeat 0)
        , unwords (take cols (zipWith fill xs rowWidths))
          : yss
        )

    fill :: String -> Int -> String
    fill s n = s ++ replicate (n - length s) ' '

strip :: String -> String
strip = dropWhileEnd (' ' ==)

concatSpaces :: String -> String
concatSpaces []        = []
concatSpaces (' ' : s) = ' ' : concatSpaces s
concatSpaces (c0 : s0)   = c0 : go s0 where
    go (' ' : ' ' : s) = go (' ' : s)
    go (c:s)           = c : go s
    go []              = []

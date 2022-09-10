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

import qualified Distribution.CabalSpecVersion             as C
import qualified Distribution.FieldGrammar                 as C
import qualified Distribution.Parsec                       as C
import qualified Distribution.Pretty                       as C
import qualified Distribution.Types.Dependency             as C
import qualified Distribution.Types.DependencyMap          as C
import qualified Distribution.Types.ExeDependency          as C
import qualified Distribution.Types.Version                as C
import qualified Distribution.Types.VersionInterval.Legacy as C
import qualified Distribution.Types.VersionRange           as C
import qualified Text.PrettyPrint                          as PP

import qualified Distribution.Types.VersionInterval.Legacy as C ()

import CabalFmt.Fields
import CabalFmt.Options
import CabalFmt.Prelude

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

pretty :: Options -> [C.Dependency] -> PP.Doc
pretty opts deps = case deps of
    []    -> PP.empty
    [dep] -> C.pretty (C.depPkgName dep) PP.<+> prettyVR vr'
      where
        vr' = either (C.fromVersionIntervals . C.mkVersionIntervals) id
            $ norm opts (C.asVersionIntervals $ C.depVerRange dep)

        prettyVR vr | vr == C.anyVersion = PP.empty
                    | vr == C.noVersion  = PP.text "<0"
                    | otherwise          = C.pretty vr

    _ -> prettyMany opts deps'
      where
        deps' :: [(String, [C.VersionInterval])]
        deps' = sortOn (map toLower . fst)
              $ map (prettyDepNoVersion &&& C.asVersionIntervals . C.depVerRange)
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
        vr' = either (C.fromVersionIntervals . C.mkVersionIntervals) id
            $ norm opts (C.asVersionIntervals $ exeDepVerRange dep)

        prettyVR vr | vr == C.anyVersion = PP.empty
                    | vr == C.noVersion  = PP.text "<0"
                    | otherwise          = C.pretty vr

    _ -> prettyMany opts deps'
      where
        deps' :: [(String, [C.VersionInterval])]
        deps' = sortOn (map toLower . fst)
              $ map (exeDepExeName &&& C.asVersionIntervals . exeDepVerRange)
              -- C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

exeDepExeName :: C.ExeDependency -> String
exeDepExeName (C.ExeDependency name exe _) =
    C.prettyShow name ++ ":" ++ C.prettyShow exe

exeDepVerRange :: C.ExeDependency -> C.VersionRange
exeDepVerRange (C.ExeDependency _ _ vr) = vr

prettyMany :: Options -> [(String, [C.VersionInterval])] -> PP.Doc
prettyMany opts@Options { optSpecVersion = v, optTabular = tab } deps'
    = PP.vcat
    $ map PP.text
    $ tbl
    $ zipWith cols (True : repeat False) deps'
  where
    cols :: Bool -> (String, [C.VersionInterval]) -> [String]
    cols isFirst (name, vis)
        | full  vis = [comma, name]
        | otherwise = case norm opts vis of
            Left []          -> [ comma, name, "", "<0"] -- empty
            Left (vi : vis') -> [ comma, name, "" ] ++ prettyVi vi ++ concat [ "||" : prettyVi vi' | vi' <- vis' ]
            Right vr         -> comma : name : "" : words (C.prettyShow vr)
      where
        comma | isFirst, v < C.CabalSpecV2_2 = " "
              | otherwise                    = ","

    full :: [C.VersionInterval] -> Bool
    full [(C.LowerBound l C.InclusiveBound, C.NoUpperBound)] = l == C.version0
    full _                                                   = False

    tbl :: [[String]] -> [String]
    tbl = if tab then table else map (concatSpaces . unwords)

    prettyVi :: (C.LowerBound, C.UpperBound) -> [String]
    prettyVi (C.LowerBound l lb, C.NoUpperBound) =
        [ prettyLowerBound lb ++ C.prettyShow l ]
    prettyVi (C.LowerBound l C.InclusiveBound, C.UpperBound u C.InclusiveBound)
        | l == u
        =  [ "==" ++ C.prettyShow l ]
    prettyVi (C.LowerBound l C.InclusiveBound, C.UpperBound u ub)
        | l == C.version0
        = [ prettyUpperBound ub ++ C.prettyShow u ]
    prettyVi (C.LowerBound l lb, C.UpperBound u ub) =
        [ prettyLowerBound lb ++ C.prettyShow l
        , "&&"
        , prettyUpperBound ub ++  C.prettyShow u
        ]

    prettyLowerBound :: C.Bound -> String
    prettyLowerBound C.InclusiveBound = ">="
    prettyLowerBound C.ExclusiveBound = ">"

    prettyUpperBound :: C.Bound -> String
    prettyUpperBound C.InclusiveBound = "<="
    prettyUpperBound C.ExclusiveBound = "<"

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

norm :: Options -> [C.VersionInterval] -> Either [C.VersionInterval] C.VersionRange
norm _ []                                                                    = Right C.noVersion
norm _ [(C.LowerBound l C.InclusiveBound, C.NoUpperBound)] | l == C.version0 = Right C.anyVersion
norm Options { optSpecVersion = v } (i:is) = maybe (Left $ i:is) Right $
    foldr1 C.unionVersionRanges <$> traverse f (i : is)
  where
    f :: C.VersionInterval -> Maybe C.VersionRange
    f (C.LowerBound l C.InclusiveBound, C.UpperBound u C.ExclusiveBound)
        | v >= C.CabalSpecV2_0, u == C.majorUpperBound l =
            Just (C.majorBoundVersion l)
    f _ = Nothing

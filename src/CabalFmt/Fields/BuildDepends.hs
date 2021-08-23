-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module CabalFmt.Fields.BuildDepends (
    buildDependsF,
    setupDependsF,
    ) where

import qualified Distribution.CabalSpecVersion      as C
import qualified Distribution.FieldGrammar          as C
import qualified Distribution.Parsec                as C
import qualified Distribution.Pretty                as C
import qualified Distribution.Types.Dependency      as C
import qualified Distribution.Types.DependencyMap   as C
import qualified Distribution.Types.PackageName     as C
import qualified Distribution.Types.Version         as C
import qualified Distribution.Types.VersionInterval.Legacy as C
import qualified Distribution.Types.VersionRange    as C
import qualified Text.PrettyPrint                   as PP

import qualified Distribution.Types.VersionInterval.Legacy as C ()

import CabalFmt.Fields
import CabalFmt.Options
import CabalFmt.Prelude

setupDependsF :: Options -> FieldDescrs () ()
setupDependsF opts = singletonF "setup-depends" (pretty opts) parse

buildDependsF :: Options -> FieldDescrs () ()
buildDependsF opts = singletonF "build-depends" (pretty opts) parse

parse :: C.CabalParsing m => m [C.Dependency]
parse = unpack' (C.alaList C.CommaVCat) <$> C.parsec

pretty :: Options -> [C.Dependency] -> PP.Doc
pretty Options { optSpecVersion = v, optTabular = tab } deps = case deps of
    []    -> PP.empty
    [dep] -> C.pretty (C.depPkgName dep) PP.<+> prettyVR vr'
      where
        vr' = either (C.fromVersionIntervals . C.mkVersionIntervals) id
            $ norm (C.asVersionIntervals $ C.depVerRange dep)

        prettyVR vr | vr == C.anyVersion = PP.empty
                    | vr == C.noVersion  = PP.text "<0"
                    | otherwise          = C.pretty vr

    _ -> PP.vcat (zipWith pretty' (True : repeat False) deps')
      where
        deps' :: [(String, [C.VersionInterval])]
        deps' = sortOn (map toLower . fst)
              $ map (C.unPackageName . C.depPkgName &&& C.asVersionIntervals . C.depVerRange)
              $ C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

        width = maximum (0 : map (length . fst) deps') + 1
        width' = maximum (0 : map (firstComponent . snd) deps')

        -- we assume cabal-version: 2.2 or higher
        pretty' :: Bool -> (String, [C.VersionInterval]) -> PP.Doc
        pretty' isFirst (name, vis)
            | empty vis = comma PP.<+> PP.text name PP.<+> PP.text "<0"
            | full  vis = comma PP.<+> PP.text name
            | otherwise = case norm vis of
                Left [] -> comma PP.<+> PP.text name
                Left (vi : vis') ->
                    comma PP.<+>
                    PP.text (lp width name) PP.<+>
                    PP.hsep
                        ( prettyVi vi
                        : map (\vi' -> PP.text "||" PP.<+> prettyVi' vi') vis'
                        )
                Right vr ->
                    comma PP.<+>
                    PP.text (lp width name) PP.<+>
                    C.pretty vr
          where
            comma | isFirst, v < C.CabalSpecV2_2 = PP.text " "
                  | otherwise = PP.comma

        -- indent first
        prettyVi (C.LowerBound l lb, C.NoUpperBound) =
            prettyLowerBound lb PP.<> C.pretty l
        prettyVi (C.LowerBound l C.InclusiveBound, C.UpperBound u C.InclusiveBound)
            | l == u = PP.text "==" PP.<> C.pretty l
        prettyVi (C.LowerBound l C.InclusiveBound, C.UpperBound u ub)
            | l == C.version0
            = prettyUpperBound ub PP.<> C.pretty u
        prettyVi (C.LowerBound l lb, C.UpperBound u ub) =
            prettyLowerBound lb PP.<> PP.text (lp width' l')
            PP.<+> PP.text "&&" PP.<+>
            prettyUpperBound ub PP.<> C.pretty u
          where
            l' = C.prettyShow l

        prettyVi' (C.LowerBound l lb, C.NoUpperBound) =
            prettyLowerBound lb PP.<> C.pretty l
        prettyVi' (C.LowerBound l C.InclusiveBound, C.UpperBound u C.InclusiveBound)
            | l == u = PP.text "==" PP.<> C.pretty l
        prettyVi' (C.LowerBound l lb, C.UpperBound u ub) =
            prettyLowerBound lb PP.<> C.pretty l PP.<+> PP.text "&&" PP.<+>
            prettyUpperBound ub PP.<> C.pretty u

        prettyLowerBound :: C.Bound -> PP.Doc
        prettyLowerBound C.InclusiveBound = PP.text ">="
        prettyLowerBound C.ExclusiveBound = PP.text ">"

        prettyUpperBound :: C.Bound -> PP.Doc
        prettyUpperBound C.InclusiveBound = PP.text "<="
        prettyUpperBound C.ExclusiveBound = PP.text "<"
  where
    full :: [C.VersionInterval] -> Bool
    full [(C.LowerBound l C.InclusiveBound, C.NoUpperBound)] = l == C.mkVersion [0]
    full _                                                   = False

    empty :: [C.VersionInterval] -> Bool
    empty [] = True
    empty _  = False

    norm :: [C.VersionInterval] -> Either [C.VersionInterval] C.VersionRange
    norm []                                                                    = Right C.noVersion
    norm [(C.LowerBound l C.InclusiveBound, C.NoUpperBound)] | l == C.version0 = Right C.anyVersion
    norm (i:is) = maybe (Left $ i:is) Right $
        foldr1 C.unionVersionRanges <$> traverse f (i : is)
      where
        f :: C.VersionInterval -> Maybe C.VersionRange
        f (C.LowerBound l C.InclusiveBound, C.UpperBound u C.ExclusiveBound)
            | v >= C.CabalSpecV2_0, u == C.majorUpperBound l =
                Just (C.majorBoundVersion l)
        f _ = Nothing

    firstComponent :: [C.VersionInterval] -> Int
    firstComponent [] = 0
    firstComponent ((C.LowerBound l _, _) : _) = length (C.prettyShow l)

    lp | tab       = leftpad
       | otherwise = \_ x -> x

leftpad :: Int -> String -> String
leftpad w s = s ++ replicate (w - length s) ' '

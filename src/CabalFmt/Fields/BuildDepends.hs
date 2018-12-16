{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Fields.BuildDepends (
    buildDependsF,
    ) where

import Control.Arrow               ((&&&))
import Data.Char                   (toLower)
import Data.List                   (sortOn)
import Distribution.Compat.Newtype

import qualified Distribution.CabalSpecVersion      as C
import qualified Distribution.Parsec                as C
import qualified Distribution.Parsec.Newtypes       as C
import qualified Distribution.Pretty                as C
import qualified Distribution.Types.Dependency      as C
import qualified Distribution.Types.DependencyMap   as C
import qualified Distribution.Types.PackageName     as C
import qualified Distribution.Types.VersionInterval as C
import qualified Distribution.Types.VersionRange    as C
import qualified Text.PrettyPrint                   as PP

import CabalFmt.Fields

buildDependsF :: C.CabalSpecVersion -> FieldDescrs () ()
buildDependsF v = singletonF "build-depends" pretty parse where
    parse :: C.CabalParsing m => m [C.Dependency]
    parse = unpack' (C.alaList C.CommaVCat) <$> C.parsec

    pretty :: [C.Dependency] -> PP.Doc
    pretty [] = PP.empty
    pretty [dep] = C.pretty (C.depPkgName dep) PP.<+> prettyVR (norm (C.depVerRange dep))
      where
        prettyVR vr | vr == C.anyVersion = PP.empty
                    | otherwise          = C.pretty vr
    pretty deps = PP.vcat (zipWith pretty' (True : repeat False) deps') where
        deps' = sortOn (map toLower . fst)
              $ map (C.unPackageName . C.depPkgName &&& C.depVerRange)
              $ C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

        width = maximum (0 : map (length . fst) deps') + 1

        -- we assume cabal-version: 2.2 or higher
        pretty' :: Bool -> (String, C.VersionRange) -> PP.Doc
        pretty' isFirst (name, vr)
            | vr == C.anyVersion = comma PP.<+> PP.text name
            | otherwise =
                comma PP.<+>
                PP.text (leftpad width name) PP.<+>
                C.pretty (norm vr)
          where
            comma | isFirst, v < C.CabalSpecV2_2 = PP.text " "
                  | otherwise = PP.comma

    norm :: C.VersionRange -> C.VersionRange
    norm vr = case C.asVersionIntervals vr of
        []     -> vr
        (i:is) -> foldl C.unionVersionRanges (f i) (map f is)
      where
        f (C.LowerBound l C.InclusiveBound, C.UpperBound u C.ExclusiveBound)
            | v >= C.CabalSpecV2_0, u == C.majorUpperBound l = C.majorBoundVersion l
        f i' = C.fromVersionIntervals $ C.mkVersionIntervals [i']

leftpad :: Int -> String -> String
leftpad w s = s ++ replicate (w - length s) ' '

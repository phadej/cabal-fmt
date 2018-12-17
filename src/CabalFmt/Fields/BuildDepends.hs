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
import qualified Distribution.Types.Version         as C
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
    pretty [dep] =
        C.pretty (C.depPkgName dep) PP.<+>
        prettyVR vr'
      where
        vr' = either (C.fromVersionIntervals . C.mkVersionIntervals) id
            $ norm (C.asVersionIntervals $ C.depVerRange dep)
        prettyVR vr | vr == C.anyVersion = PP.empty
                    | otherwise          = C.pretty vr
    pretty deps = PP.vcat (zipWith pretty' (True : repeat False) deps') where
        deps' = sortOn (map toLower . fst)
              $ map (C.unPackageName . C.depPkgName &&& C.asVersionIntervals . C.depVerRange)
              $ C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

        width = maximum (0 : map (length . fst) deps') + 1
        width' = maximum (0 : map (firstComponent . snd) deps')

        -- we assume cabal-version: 2.2 or higher
        pretty' :: Bool -> (String, [C.VersionInterval]) -> PP.Doc
        pretty' isFirst (name, vis)
            | empty vis = comma PP.<+> PP.text name
            | otherwise = case norm vis of
                Left [] -> comma PP.<+> PP.text name
                Left (vi : vis') ->
                    comma PP.<+>
                    PP.text (leftpad width name) PP.<+>
                    PP.hsep
                        ( prettyVi vi
                        : map (\vi' -> PP.text "||" PP.<+> prettyVi' vi') vis'
                        )
                Right vr ->
                    comma PP.<+>
                    PP.text (leftpad width name) PP.<+>
                    C.pretty vr
          where
            comma | isFirst, v < C.CabalSpecV2_2 = PP.text " "
                  | otherwise = PP.comma

        -- indent first
        prettyVi (C.LowerBound l lb, C.NoUpperBound) =
            prettyLowerBound lb PP.<> C.pretty l
        prettyVi (C.LowerBound l C.InclusiveBound, C.UpperBound u C.InclusiveBound)
            | l == u = PP.text "==" PP.<> C.pretty l
        prettyVi (C.LowerBound l lb, C.UpperBound u ub) =
            prettyLowerBound lb PP.<> PP.text (leftpad width' l')
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

    empty :: [C.VersionInterval] -> Bool
    empty []                                                  = True
    empty [(C.LowerBound l C.InclusiveBound, C.NoUpperBound)] = l == C.mkVersion [0]
    empty _                                                   = False

    norm :: [C.VersionInterval] -> Either [C.VersionInterval] C.VersionRange
    norm []     = Right C.anyVersion
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

leftpad :: Int -> String -> String
leftpad w s = s ++ replicate (w - length s) ' '

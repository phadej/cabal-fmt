{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Monad                   (replicateM)
import Distribution.Parsec             (eitherParsec)
import Distribution.Types.Version
       (Version, mkVersion, validVersion, versionNumbers)
import Distribution.Types.VersionRange
       (VersionRange, VersionRangeF (..), anyVersion, earlierVersion,
       intersectVersionRanges, laterVersion, majorBoundVersion, notThisVersion,
       orEarlierVersion, orLaterVersion, projectVersionRange, thisVersion,
       unionVersionRanges, withinRange, withinVersion)
import Distribution.Version            (transformCaretUpper)
import Math.NumberTheory.Logarithms    (intLog2)
import Test.QuickCheck                 (Arbitrary (..), (===))
import Test.Tasty                      (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit                (testCase, (@?=))
import Test.Tasty.QuickCheck           (testProperty)

import qualified Test.QuickCheck as QC

import VersionInterval

main :: IO ()
main = defaultMain $ testGroup "version-interval"
    [ testGroup "Validity"
        [ testProperty "validVersion" validVersion
        , testProperty "validVersionInterval" validVersionInterval
        , testProperty "validVersionIntervals" $ \vr ->
            let intervals = toVersionIntervals vr
            in QC.counterexample (show intervals) $ validVersionIntervals intervals
        ]

    , testGroup "VersionInterval"
        [ testProperty "intersect valid" $ \a b ->
            let ab = intersectInterval a b
            in maybe (QC.property True) (\ab' -> QC.counterexample ("intersection: " ++ show ab') $ validVersionInterval ab') ab

        , testProperty "intersect complete" intersect_complete
        , testProperty "intersect complete lax" intersect_complete_lax
        ]

    , testGroup "stage1"
        [ testProperty "valid" stage1_valid
        , testProperty "complete" stage1_complete
        , testProperty "complete lax" stage1_complete_lax

        , testProperty "valid ex1" $ stage1_valid
            (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (thisVersion (mkVersion [1])))

        , testProperty "valid ex2" $ stage1_valid
            (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1])))

        , testProperty "complete ex1" $ stage1_complete
            (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (thisVersion (mkVersion [6])))
            (mkVersion [6])

        , testProperty "complete ex2" $ stage1_complete
            (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1])))

        , testProperty "complete lax ex2" $ stage1_complete_lax
            (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1])))
        ]

    , testGroup "stage2"
        [ testProperty "valid" stage2_valid
        , testProperty "involutive" stage2_involutive
        , testProperty "complete" stage2_complete
        , testProperty "complete_lax" stage2_complete_lax
        ]

    , testGroup "stage3"
        [ testProperty "valid" stage3_valid
        , testProperty "involutive" stage3_involutive
        , testProperty "complete" stage3_complete
        , testProperty "complete_lax" stage3_complete_lax

        , testProperty "complete ex1" $ stage3_complete
           [VI (LB (mkVersion [0])) (MB (mkVersion [0])) NoUB,VI (LB (mkVersion [0,0])) NoMB NoUB]
           (mkVersion [0])

        , testProperty "complete lax ex1" $ stage3_complete_lax
            [VI (LB (mkVersion [0,0])) (MB (mkVersion [0])) (UB (mkVersion [1])),VI (LB (mkVersion [0,0,0])) (MB (mkVersion [1])) (UB (mkVersion [1]))]
            (mkVersion [1])

        , testProperty "valid ex2" $ stage3_valid
            (stage1 id (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1]))))

        , testProperty "complete ex2" $ stage3_complete
            (stage1 id (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1]))))

        , testProperty "complete lax ex2" $ stage3_complete_lax
            (stage1 id (intersectVersionRanges (majorBoundVersion (mkVersion [0])) (orLaterVersion (mkVersion [1]))))

        , testProperty "valid ex3" $ stage3_valid
            [VI (LB (mkVersion [0,0])) (MB (mkVersion [0])) NoUB,VI (LB (mkVersion [0,0])) NoMB NoUB]
        ]

    , testGroup "normalise"
        [ normaliseExample ">=1 && <2"                               ">=1 && <2"
        , normaliseExample "^>=1"                                    "^>=1"
        , normaliseExample "^>=1 || ^>=2"                            "^>=1 || ^>=2"
        , normaliseExample "^>=1.2 || ^>=1.3 || ^>=1.4"              "^>=1.2 || ^>=1.3 || ^>=1.4"
        , normaliseExample "^>=1.2 || ^>=2.0"                        "^>=1.2 || ^>=2.0"
        , normaliseExample ">=1.2 && <1.4 || ^>=1.4 || ^>=1.5"       "^>=1.2 || ^>=1.3 || ^>=1.4 || ^>=1.5"
        , normaliseExample ">=1.2 && <2.4 || ^>=2.4 || ^>=2.5"       ">=1.2 && <2.5 || ^>=2.5"
        , normaliseExample "^>=1.2.0.0 || ^>=1.3.0.0 || ^>=1.4.0.0"  "^>=1.2.0.0 || ^>=1.3.0.0 || ^>=1.4.0.0"

        , normaliseExample "==3.1.4"           "==3.1.4"
        , normaliseExample "<3.1.4 || >3.1.4"  "<3.1.4 || >=3.1.4.0"

        , normaliseExample "^>=1.2 && <2.0 || ^>=2.0"                "^>=1.2 || ^>=2.0"
        , normaliseExample "^>=1.2 && <3.0 || ^>=2.0"                "^>=1.2 || ^>=2.0"
        , normaliseExample "^>=1.2 || ^>=2.0 && <3"                  "^>=1.2 || ^>=2.0"
        , normaliseExample "(^>=1.2 || ^>=2.0) && <3"                "^>=1.2 && <2.0 || ^>=2.0 && <3"
        , normaliseExample "(>=1.2 || >=2.0) && <3"                  ">=1.2 && <3"

        , normaliseExample "<1 || >=1.0"   "<1 || >=1.0"
        , normaliseExample "<=1 || >=1.0"  ">=0"
        , normaliseExample ">0 && <0.0"    "<0"

        , normaliseExample "^>=1.5.0.1 || ^>=1.6.0.1 || >=1.9 && <1.13"
                           "^>=1.5.0.1 || ^>=1.6.0.1 || ^>=1.9 || ^>=1.10 || ^>=1.11 ||^>=1.12"

        , cannotNormaliseExample "^>=0 && >=0.1" IntervalsEmpty

        , testProperty "involutive"   normalise_involutive

        , testProperty "complete"     normalise_complete
        , testProperty "complete_lax" normalise_complete_lax

        , testProperty "involutive ex1" $ normalise_involutive $
            intersectVersionRanges (majorBoundVersion (mkVersion [3])) (laterVersion (mkVersion [3]))
        ]
    ]

normaliseExample :: String -> String -> TestTree
normaliseExample input expected = testCase input $ do
    input'   <- either fail return $ eitherParsec input
    expected' <- either fail return $ eitherParsec expected

    VersionInterval.normaliseVersionRange input' @?= Right expected'

cannotNormaliseExample :: String -> ConversionProblem -> TestTree
cannotNormaliseExample input problem = testCase ("cannot " ++ input) $ do
    input'   <- either fail return $ eitherParsec input

    VersionInterval.normaliseVersionRange input' @?= Left problem

-------------------------------------------------------------------------------
-- Intersect
-------------------------------------------------------------------------------

intersect_complete :: VersionInterval -> VersionInterval -> Version -> QC.Property
intersect_complete a b v =
    QC.counterexample ("intersection: " ++ show (ab, inA, inB)) $ (inA && inB) === inAB
  where
    ab   = intersectInterval a b
    inA  = withinInterval v a
    inB  = withinInterval v b
    inAB = maybe False (withinInterval v) ab

intersect_complete_lax :: VersionInterval -> VersionInterval -> Version -> QC.Property
intersect_complete_lax a b v =
    QC.counterexample ("intersection: " ++ show (ab, inA, inB)) $ (inA && inB) === inAB
  where
    ab   = intersectInterval a b
    inA  = withinIntervalLax v a
    inB  = withinIntervalLax v b
    inAB = maybe False (withinIntervalLax v) ab

-------------------------------------------------------------------------------
-- Stage 1
-------------------------------------------------------------------------------

stage1_valid :: VersionRange -> QC.Property
stage1_valid vr = QC.counterexample ("stage1: " ++ show is) $ QC.property $ all validVersionInterval is
  where
    is = stage1 id vr

stage1_complete :: VersionRange -> Version -> QC.Property
stage1_complete vr v = QC.counterexample ("stage1: " ++ show is) $ withinRange v vr === any (withinInterval v) is
  where
    is = stage1 id vr

stage1_complete_lax :: VersionRange -> Version -> QC.Property
stage1_complete_lax vr v = QC.counterexample ("stage1: " ++ show is) $ withinRangeLax v vr === any (withinIntervalLax v) is
  where
    is = stage1 id vr

-------------------------------------------------------------------------------
-- Stage 2
-------------------------------------------------------------------------------

stage2_valid :: [VersionInterval] -> QC.Property
stage2_valid is' = QC.counterexample ("stage2: " ++ show is) $ QC.property $ all validVersionInterval is
  where
    is = stage2 is'

stage2_involutive :: [VersionInterval] -> QC.Property
stage2_involutive is' = is1 === is2
  where
    is1 = stage2 is'
    is2 = stage2 is1

stage2_complete :: [VersionInterval] -> Version -> QC.Property
stage2_complete is' v = QC.counterexample ("stage2: " ++ show is) $ any (withinInterval v) is' === any (withinInterval v) is
  where
    is = stage2 is'

stage2_complete_lax :: [VersionInterval] -> Version -> QC.Property
stage2_complete_lax is' v = QC.counterexample ("stage2: " ++ show is) $ any (withinIntervalLax v) is' === any (withinIntervalLax v) is
  where
    is = stage2 is'

-------------------------------------------------------------------------------
-- Stage 3
-------------------------------------------------------------------------------

stage3_valid :: [VersionInterval] -> QC.Property
stage3_valid is' = QC.counterexample ("stage3: " ++ show is) $ QC.property $ all validVersionInterval is
  where
    is = stage3 (stage2 is')

stage3_involutive :: [VersionInterval] -> QC.Property
stage3_involutive is' = is1 === is2
  where
    is1 = stage3 (stage2 is')
    is2 = stage3 is1

stage3_complete :: [VersionInterval] -> Version -> QC.Property
stage3_complete is' v = QC.counterexample ("stage3: " ++ show is) $ any (withinInterval v) is' === any (withinInterval v) is
  where
    is = stage3 (stage2 is')

stage3_complete_lax :: [VersionInterval] -> Version -> QC.Property
stage3_complete_lax is' v = QC.counterexample ("stage3: " ++ show is) $ any (withinIntervalLax v) is' === any (withinIntervalLax v) is
  where
    is = stage3 (stage2 is')

-------------------------------------------------------------------------------
-- Normalise
-------------------------------------------------------------------------------

normalise_involutive :: VersionRange -> QC.Property
normalise_involutive vr = vr1 === vr2 where
    vr1 = VersionInterval.normaliseVersionRange vr
    vr2 = VersionInterval.normaliseVersionRange =<< vr1

normalise_complete :: VersionRange -> Version -> QC.Property
normalise_complete vr v = case VersionInterval.normaliseVersionRange vr of
    Left _    -> QC.property True
    Right vr' -> QC.counterexample ("normalised: " ++ show vr') $ withinRange v vr === withinRange v vr'

normalise_complete_lax :: VersionRange -> Version -> QC.Property
normalise_complete_lax vr v = case VersionInterval.normaliseVersionRange vr of
    Left _    -> QC.property True
    Right vr' -> QC.counterexample ("normalised: " ++ show vr') $ withinRangeLax v vr === withinRangeLax v vr'

-------------------------------------------------------------------------------
-- Predicates
-------------------------------------------------------------------------------

withinInterval :: Version -> VersionInterval -> Bool
withinInterval v (VI l m u) = viGreater l && viLessM m && viLess u where
    viGreater (LB v') = v >= v'

    viLessM NoMB    = True
    viLessM (MB v') = v < v'

    viLess NoUB    = True
    viLess (UB v') = v < v'

withinIntervalLax :: Version -> VersionInterval -> Bool
withinIntervalLax v (VI l _ u) = viGreater l && viLess u where
    viGreater (LB v') = v >= v'

    viLess NoUB    = True
    viLess (UB v') = v < v'

withinRangeLax :: Version -> VersionRange -> Bool
withinRangeLax v vr = withinRange v (transformCaretUpper vr)

-------------------------------------------------------------------------------
-- QC instances
-------------------------------------------------------------------------------

instance Arbitrary Version where
    arbitrary = QC.oneof
        [ mkVersion <$> replicateM d vDigit
        | d <- [1..4]
        ]
      where
        vDigit :: QC.Gen Int
        vDigit = QC.elements [0..9]

    shrink v =
        [ mkVersion v'
        | v' <- shrink (versionNumbers v)
        , not (null v')
        ]

instance Arbitrary VersionRange where
    arbitrary = QC.sized $ \n -> genVersionRange (intLog2 (max 1 n))

    shrink vr = case projectVersionRange vr of
        LaterVersionF v -> laterVersion <$> shrink v
        OrLaterVersionF v -> orLaterVersion <$> shrink v
        EarlierVersionF v -> earlierVersion <$> shrink v
        OrEarlierVersionF v -> orEarlierVersion <$> shrink v
        ThisVersionF v -> thisVersion <$> shrink v
        MajorBoundVersionF v -> majorBoundVersion <$> shrink v
        UnionVersionRangesF l r -> l : r : fmap (uncurry unionVersionRanges) (shrink (l, r))
        IntersectVersionRangesF l r -> l : r : fmap (uncurry intersectVersionRanges) (shrink (l, r))

genVersionRange :: Int -> QC.Gen VersionRange
genVersionRange n
    | n <= 1 = QC.oneof
        [ pure anyVersion
        , thisVersion <$> arbitrary
        , notThisVersion <$> arbitrary
        , laterVersion <$> arbitrary
        , earlierVersion <$> arbitrary
        , orLaterVersion <$> arbitrary
        , orEarlierVersion <$> arbitrary
        , withinVersion  <$> arbitrary
        , majorBoundVersion <$> arbitrary
        ]
    | otherwise = do
        l <- QC.chooseInt (1, n - 1)
        let r = n - l
        QC.oneof
            [ unionVersionRanges <$> genVersionRange l <*> genVersionRange r
            , intersectVersionRanges <$> genVersionRange l <*> genVersionRange r
            ]

instance Arbitrary VersionInterval where
    arbitrary = QC.suchThat (VI <$> arbitrary <*> arbitrary <*> arbitrary) validVersionInterval

    shrink (VI l m u) =
        [ vi
        | (l', m', u') <- shrink (l, m, u)
        , let vi = VI l' m' u'
        , validVersionInterval vi
        ]

instance Arbitrary LB where
    arbitrary = LB <$> arbitrary
    shrink (LB v) = LB <$> shrink v

instance Arbitrary UB where
    arbitrary = QC.oneof
        [ pure NoUB
        , UB <$> arbitrary
        ]

    shrink NoUB     = []
    shrink (UB v) = NoUB : map UB (shrink v)

instance Arbitrary MB where
    arbitrary = QC.oneof
        [ pure NoMB
        , MB <$> arbitrary
        ]

    shrink NoMB     = []
    shrink (MB v) = NoMB : map MB (shrink v)

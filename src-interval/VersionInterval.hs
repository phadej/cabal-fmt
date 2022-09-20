{-# LANGUAGE BangPatterns #-}

-- |
--
-- Author:          Oleg Grenrus <oleg.grenrus@iki.fi>
-- SPDX-License-Id: GPL-2.0-or-later
--
-- This module implements a view of a 'VersionRange' as a finite
-- list of separated version intervals.
--
-- In conversion from and to 'VersionRange' it makes some effort to
-- preserve the caret operator @^>=x.y@.  This constraint a priori
-- specifies the same interval as @==x.y.*@, but indicates that newer
-- versions could be acceptable (@allow-newer: ^@).
--
module VersionInterval (
    -- * Version intervals
    VersionIntervals,
    unVersionIntervals,

    -- * Conversions
    toVersionIntervals,
    fromVersionIntervals,
    ConversionProblem (..),

    -- ** Normalisation
    normaliseVersionRange,

    -- * Version intervals view
    VersionInterval (..),
    LB(..),
    MB(..),
    UB(..),
    Bound(..),

    -- * For testing
    validVersionInterval,
    validVersionIntervals,
    intersectInterval,
    stage1, stage2, stage3,
)  where

import Control.Applicative (liftA2)
import Control.Monad       (join)
import Data.List           (sortOn)
import Data.List.NonEmpty  (NonEmpty (..), cons)
import Data.Maybe          (catMaybes)

import Distribution.Types.Version
       (Version, mkVersion, validVersion, version0, versionNumbers)
import Distribution.Types.VersionRange.Internal
       (VersionRange, VersionRangeF (..), cataVersionRange, earlierVersion,
       intersectVersionRanges, majorBoundVersion, majorUpperBound, noVersion,
       orLaterVersion, thisVersion, unionVersionRanges)

singleton :: a -> NonEmpty a
singleton x = x :| []

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | A complementary representation of a 'VersionRange'. Instead of a boolean
-- version predicate it uses an increasing sequence of non-overlapping,
-- non-empty intervals.
--
-- This version is different than in @Cabal-3.8@ and previous,
-- as it tries to preserve @^>=@ version ranges under default and @transformCaretUpper@ semantics.
-- Slighly simplifying, 'normalizeVersionRange' shouldn't destroy @^>=@ in version range expressions.
--
newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show)

-- | Inspect the list of version intervals.
--
unVersionIntervals :: VersionIntervals -> [VersionInterval]
unVersionIntervals (VersionIntervals is) = is

-- | Version interval.
--
-- Invariants:
--
-- * Interval is non-empty
-- * 'MB' is between 'LB' and 'UB'.
--
data VersionInterval = VI !LB !MB !UB
  deriving (Eq, Show)

-- | Lower bound. For intervals it always exist: 'zeroLB' i.e. @>= 0@.
--
-- All lower bound intervals are inclusive, i.e. @>=v@. @>x.y.z@ is converted into @>=x.y.z.0@.
--
data LB = LB !Version
  deriving (Eq, Ord, Show)

-- | Upper bound.
--
-- All upper bounds are exclusive, i.e. @<v@. @<=x.y.z@ is converted to @<x.y.z.0@.
--
--
data UB
    = UB !Version   -- ^ upper bound
    | NoUB          -- ^ no upper bound (i.e. infinite)
  deriving (Eq, Ord, Show)

-- | Bound variant.
data Bound
    = Incl  -- ^ inclusive: @>=@ or @<=@
    | Excl  -- ^ exclusive: @>@ or @<@
  deriving (Eq, Ord, Show)

-- | Middle bound.
data MB
    = MB !Version  -- ^ major bound.
    | NoMB         -- ^ no major bound (i.e. infinite)
  deriving (Eq, Ord, Show)

-- | @>=0@
zeroLB :: LB
zeroLB = LB version0

-- | Whether the version is @0@.
isVersion0 :: Version -> Bool
isVersion0 = (==) version0

-- | Versions are not separated type.
succVersion :: Version -> Version
succVersion v = mkVersion (versionNumbers v ++ [0])

-------------------------------------------------------------------------------
-- Stage1
-------------------------------------------------------------------------------

stage1 :: ([VersionInterval] -> [VersionInterval]) -> VersionRange -> [VersionInterval]
stage1 opt = cataVersionRange alg where
    -- version range leafs transform into singleton intervals
    alg (ThisVersionF v)                = [VI (LB v)                (MB (succVersion v))      (UB (succVersion v))]
    alg (LaterVersionF v)               = [VI (LB (succVersion v))  NoMB                      NoUB]
    alg (OrLaterVersionF v)             = [VI (LB v)                NoMB                      NoUB]
    alg (EarlierVersionF v)
        | isVersion0 v                  = []
        | otherwise                     = [VI zeroLB                (MB v)                    (UB v)]
    alg (OrEarlierVersionF v)           = [VI zeroLB                (MB (succVersion v))      (UB (succVersion v))]

    -- ^>= version-range's upper bound should be MajorBound
    alg (MajorBoundVersionF v)          = [VI (LB v)                (MB (majorUpperBound v))  NoUB]

    -- union: just merge the version intervals
    alg (UnionVersionRangesF v1 v2)     = v1 ++ v2

    -- intersection: pairwise intersect. Strip empty intervals. Sort to restore the invariant.
    alg (IntersectVersionRangesF v1 v2) = catMaybes $ liftA2 intersectInterval (opt v1) (opt v2)

-------------------------------------------------------------------------------
-- Stage2
-------------------------------------------------------------------------------

stage2 :: [VersionInterval] -> [VersionInterval]
stage2 = sortOn (\(VI l _ _) -> l)

-------------------------------------------------------------------------------
-- Postprocess
-------------------------------------------------------------------------------

stage2and3 :: [VersionInterval] -> [VersionInterval]
stage2and3 = stage3 . stage2

stage3 :: [VersionInterval] -> [VersionInterval]
stage3 []                = []
stage3 (VI l m u : rest) = stage3go l m u rest

stage3go :: LB -> MB -> UB -> [VersionInterval] -> [VersionInterval]
stage3go l m u []                 = [VI l m u]
stage3go l m u (VI l' m' u' : is)
    | l == l'   = stage3go l' (unionMB m m') (unionUB u u') is
    | otherwise = case overlap m u l' of
        NoOverlap -> VI l m u : stage3go l' m' u' is
        OverlapU  -> viCons (VI l m (trimLB u l')) (stage3go l' m' (unionUB u u') is)
        OverlapM  -> stage3go l (unionMB m m') (unionUB u u') is
  where
    viCons :: VersionInterval -> [VersionInterval] -> [VersionInterval]
    viCons i | nonEmptyVI i = (i :)
    viCons _                = id

trimLB :: UB -> LB -> UB
trimLB _    (LB l) = UB l

-------------------------------------------------------------------------------
-- Intersections
-------------------------------------------------------------------------------

intersectInterval :: VersionInterval -> VersionInterval -> Maybe VersionInterval
intersectInterval (VI xl xm xu) (VI yl ym yu)
    | nonEmptyVI xy = Just xy
    | otherwise     = Nothing
  where
    l = intersectLB xl yl
    m = intersectMB xm ym
    u = intersectUB xu yu

    -- make middle bound be between l and u
    m' = rtrimMB (ltrimMB l m) u

    xy = VI l m' u

ltrimMB :: LB -> MB -> MB
ltrimMB _         NoMB      = NoMB
ltrimMB (LB l) (MB m) = case compare l m of
    LT -> MB m
    EQ -> MB m
    GT -> MB l

rtrimMB :: MB -> UB -> MB
rtrimMB m      NoUB   = m
rtrimMB NoMB   (UB u) = MB u
rtrimMB (MB m) (UB u) = MB (min m u)

intersectLB :: LB -> LB -> LB
intersectLB (LB v) (LB u) = LB (max v u)

intersectMB :: MB -> MB -> MB
intersectMB NoMB   b      = b
intersectMB b      NoMB   = b
intersectMB (MB v) (MB u) = MB (min v u)

intersectUB :: UB -> UB -> UB
intersectUB NoUB   b      = b
intersectUB b      NoUB   = b
intersectUB (UB v) (UB u) = UB (min v u)

intersectMBandUB :: MB -> UB -> UB
intersectMBandUB NoMB   b      = b
intersectMBandUB (MB v) NoUB   = UB v
intersectMBandUB (MB v) (UB u) = UB (min v u)

-------------------------------------------------------------------------------
-- Unions
-------------------------------------------------------------------------------

unionMB :: MB -> MB -> MB
unionMB NoMB   _      = NoMB
unionMB _      NoMB   = NoMB
unionMB (MB v) (MB u) = MB (max v u)

unionUB :: UB -> UB -> UB
unionUB NoUB   _      = NoUB
unionUB _      NoUB   = NoUB
unionUB (UB v) (UB u) = UB (max v u)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Overlaps.
--
data Overlap
    = NoOverlap  -- ^ no overlap, next interval's @l@ is greater than @u@
    | OverlapM   -- ^ overlaps, next interval's @l@ is less than @m@
    | OverlapU   -- ^ overlaps, next interval's @l@ is less than @u@ (but greater than @m@)
  deriving (Eq, Show)

overlap :: MB -> UB -> LB -> Overlap
overlap _           (UB u ) (LB l) | u <  l = NoOverlap
overlap (MB m ) _           (LB l) | m <  l = OverlapU
overlap _           _           _           = OverlapM

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

-- | 'VersionIntervals' invariant:
--
-- * all intervals are valid (lower bound is less then upper bound, middle bound is in between)
-- * intervals doesn't touch each other (distinct)
--
validVersionIntervals :: VersionIntervals -> Bool
validVersionIntervals (VersionIntervals intervals) =
    all validVersionInterval intervals &&
    all doesNotTouch' (pairs intervals)
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' (VI l m u, VI l' _ _) = l < l' && case overlap m u l' of
        NoOverlap -> True
        OverlapM  -> False
        OverlapU  -> case u of
            NoUB     -> True
            UB uv -> case l' of LB lv -> uv == lv

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

validLB :: LB -> Bool
validLB (LB v) = validVersion v

validUB :: UB -> Bool
validUB NoUB   = True
validUB (UB v) = validVersion v

validMB :: MB -> Bool
validMB NoMB   = True
validMB (MB v) = validVersion v

validVersionInterval :: VersionInterval -> Bool
validVersionInterval i@(VI l m u) = validLB l && validMB m && validUB u && nonEmptyVI i && lbLessThanMB l m && mbLessThanUB m u

mbLessThanUB :: MB -> UB -> Bool
mbLessThanUB (MB m) (UB u) = m <= u
mbLessThanUB NoMB   (UB _) = False
mbLessThanUB _       NoUB  = True

lbLessThanMB :: LB -> MB -> Bool
lbLessThanMB _      NoMB   = True
lbLessThanMB (LB l) (MB m) = l <= m

-- Check an interval is non-empty
--
nonEmptyVI :: VersionInterval -> Bool
nonEmptyVI (VI (LB _) _ NoUB)   = True
nonEmptyVI (VI (LB l) _ (UB u)) = l < u

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Convert a 'VersionRange' to a sequence of version intervals.
--
toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = VersionIntervals . stage2and3 . stage1 stage2and3

data ConversionProblem
    = IntervalsEmpty
    | OtherConversionProblem
  deriving (Eq, Show)

-- | Convert a 'VersionIntervals' value back into a 'VersionRange' expression
-- representing the version intervals.
--
fromVersionIntervals :: VersionIntervals -> Either ConversionProblem VersionRange
fromVersionIntervals (VersionIntervals [])     = Right noVersion
fromVersionIntervals (VersionIntervals (x:xs)) =
    case join <$> traverse intervalToVersionRange (preprocess x xs) of
        Just vrs -> Right (foldr1 unionVersionRanges vrs)
        Nothing  -> Left $
            if all seemsEmpty (x:xs)
            then IntervalsEmpty
            else OtherConversionProblem
  where
    -- we can remove upper bounds, if they touch next interval, and the next interval doesn't have upper bound
    preprocess :: VersionInterval -> [VersionInterval] -> NonEmpty VersionInterval
    preprocess i [] = i :| []
    preprocess i@(VI l m u) (j:js) = case u' of
        NoUB | touchesUB u l' -> cons (VI l m NoUB) js'
        _                     -> cons i             js'
      where
        js'@(VI l' _ u' :| _)  = preprocess j js

    seemsEmpty :: VersionInterval -> Bool
    seemsEmpty (VI l m u) = not (nonEmptyVI (VI l NoMB (intersectMBandUB m u)))

touchesUB :: UB -> LB -> Bool
touchesUB NoUB   _      = True
touchesUB (UB u) (LB l) = u >= l

lbToVR :: LB -> VersionRange
lbToVR (LB l) = orLaterVersion l

ubToVR :: UB -> VersionRange -> VersionRange
ubToVR NoUB   vr = vr
ubToVR (UB u) vr = intersectVersionRanges vr (earlierVersion u)

mbEqUB :: MB -> UB -> Bool
mbEqUB NoMB   NoUB   = True
mbEqUB NoMB   (UB _) = False
mbEqUB (MB m) (UB u) = m == u
mbEqUB (MB _) NoUB   = False

-- return the unions of version ranges.
intervalToVersionRange :: VersionInterval -> Maybe (NonEmpty VersionRange)
intervalToVersionRange (VI l m u) | mbEqUB m u = Just (singleton (intervalToVersionRange1 l u))
intervalToVersionRange (VI l m u)              = fmap (fmap (ubToVR u)) (intervalToVersionRange2 l m)

intervalToVersionRange1 :: LB -> UB -> VersionRange
intervalToVersionRange1 (LB v) upper' = case upper' of
    NoUB
        -> lowerBound

    UB u
        | succVersion v == u
        -> thisVersion v

    UB u -> withLowerBound (makeUpperBound u)
  where
    lowerBound :: VersionRange
    lowerBound = lbToVR (LB v)

    withLowerBound :: VersionRange -> VersionRange
    withLowerBound vr
        | isVersion0 v = vr
        | otherwise    = intersectVersionRanges lowerBound vr

    makeUpperBound :: Version -> VersionRange
    makeUpperBound u = earlierVersion u

intervalToVersionRange2 :: LB -> MB -> Maybe (NonEmpty VersionRange)
intervalToVersionRange2 (LB l) major = case major of
    NoMB -> Just (singleton lowerBound)
    MB m
        | majorUpperBound l == m
        -> Just (singleton (majorBoundVersion l))

    MB m
        | [a,b]  <- versionNumbers m
        , a' : _ <- versionNumbers l
        , a' == a
        , b >= 1
        , majorUpperBound l <= m
        -> Just $ go (majorBoundVersion l :|) (majorUpperBound l)
      where
        go acc v = if v >= m then acc [] else go (acc . (majorBoundVersion v :)) (majorUpperBound v)

    MB m
        | [a,b] <- versionNumbers m
        , let m' = mkVersion [a,b-1]
        , b >= 1
        , m' > l
        -> Just $
            (ubToVR (UB m') (lbToVR (LB l)))
            :| [ majorBoundVersion (mkVersion [a, b-1]) ]

    _ -> Nothing
  where
    lowerBound :: VersionRange
    lowerBound = lbToVR (LB l)

-------------------------------------------------------------------------------
-- Normalisation
-------------------------------------------------------------------------------

-- | Convert 'VersionRange' to 'VersionIntervals' and back.
--
normaliseVersionRange :: VersionRange -> Either ConversionProblem VersionRange
normaliseVersionRange = fromVersionIntervals . toVersionIntervals

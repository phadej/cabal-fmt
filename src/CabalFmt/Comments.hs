-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module CabalFmt.Comments where

import Data.Foldable (toList)
import Data.Maybe    (fromMaybe)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Map.Strict           as Map
import qualified Distribution.Fields       as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec       as C

-------------------------------------------------------------------------------
-- Comments wrapper
-------------------------------------------------------------------------------

newtype Comments = Comments [BS.ByteString]
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

-------------------------------------------------------------------------------
-- Attach comments
-------------------------------------------------------------------------------

attachComments
    :: BS.ByteString        -- ^ source with comments
    -> [C.Field C.Position] -- ^ parsed source fields
    -> [C.Field Comments]
attachComments input inputFields = overAnn attach inputFields where
    inputFieldsU :: [(FieldPath, C.Field C.Position)]
    inputFieldsU = fieldUniverseN inputFields

    comments :: [(Int, Comments)]
    comments = extractComments input

    -- todo: warning when comments are omitted
    comments' :: Map.Map FieldPath Comments
    comments' = Map.fromListWith (flip (<>))
        [ (path, cs)
        | (l, cs) <- comments
        , path <- toList (findPath C.fieldAnn l inputFieldsU)
        ]

    attach :: FieldPath -> C.Position -> Comments
    attach fp _pos = fromMaybe mempty (Map.lookup fp comments')

overAnn :: forall a b. (FieldPath -> a -> b) -> [C.Field a] -> [C.Field b]
overAnn f = go' id where
    go :: (FieldPath -> FieldPath) -> Int -> C.Field a -> C.Field b
    go g i (C.Field (C.Name a name) fls) =
        C.Field (C.Name b name) (b <$$ fls)
      where
        b = f (g (Nth i End)) a

    go g i (C.Section (C.Name a name) args fls) =
        C.Section (C.Name b name) (b <$$ args) (go' (g . Nth i) fls)
      where
        b = f (g (Nth i End)) a

    go' :: (FieldPath -> FieldPath) -> [C.Field a] -> [C.Field b]
    go' g xs = zipWith (go g) [0..] xs

    (<$$) :: (Functor f, Functor g) => x -> f (g y) -> f (g x)
    x <$$ y = (x <$) <$> y

-------------------------------------------------------------------------------
-- Find comments in the input
-------------------------------------------------------------------------------

extractComments :: BS.ByteString -> [(Int, Comments)]
extractComments = go . zip [1..] . map (BS.dropWhile isSpace8) . BS8.lines where
    go :: [(Int, BS.ByteString)] -> [(Int, Comments)]
    go [] = []
    go ((n, bs) : rest)
        | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
            (h,t) -> (n, Comments $ bs : map snd h) : go t
        | otherwise = go rest

    (f .|| g) x = f x || g x

    isSpace8 w = w == 9 || w == 32

    isComment :: BS.ByteString -> Bool
    isComment = BS.isPrefixOf "--"

-------------------------------------------------------------------------------
-- FieldPath
-------------------------------------------------------------------------------

-- | Paths input paths. Essentially a list of offsets. Own type ofr safety.
data FieldPath
    = End
    | Nth Int FieldPath -- nth field
  deriving (Eq, Ord, Show)

fieldPathSize :: FieldPath -> Int
fieldPathSize = go 0 where
    go !acc End = acc
    go !acc (Nth _ fp) = go (succ acc) fp

fieldUniverseN :: [C.Field ann] -> [(FieldPath, C.Field ann)]
fieldUniverseN = concat . zipWith g [0..] where
    g n f' = [ (Nth n p, f'') | (p, f'') <- fieldUniverse f' ]

fieldUniverse :: C.Field ann -> [(FieldPath, C.Field ann)]
fieldUniverse f@(C.Section _ _ fs) = (End,f) : concat (zipWith g [0..] fs) where
    g n f' = [ (Nth n p, f'') | (p, f'') <- fieldUniverse f' ]
fieldUniverse f@(C.Field _ _)      = [(End, f)]

-- note: fieldUniverse* should produce 'FieldPath's in increasing order
-- that helps
findPath :: (a -> C.Position) -> Int -> [(FieldPath, a)] -> Maybe FieldPath
findPath _ _ [] = Nothing
findPath f l [(p, x)]
    | C.Position k _ <- f x =
        if l < k then Just p else Nothing
findPath f l ((_, x) : rest@((p, x') : _))
    | C.Position k  _ <- f x
    , C.Position k' _ <- f x' =
        if k < l && l < k'
        then Just p
        else findPath f l rest

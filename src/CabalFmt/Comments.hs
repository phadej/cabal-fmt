{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Comments where

import Control.Monad.Reader (asks)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import qualified Distribution.Fields       as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec       as C
import qualified Distribution.Simple.Utils as C (fromUTF8BS, toUTF8BS)

import CabalFmt.Error
import CabalFmt.Monad
import CabalFmt.Options
import CabalFmt.Parser

copyComments
    :: BS.ByteString        -- ^ source with comments
    -> [C.Field C.Position] -- ^ parsed source fields
    -> String               -- ^ new source (without comments, but same structure)
    -> CabalFmt String      -- ^ new source with comments
copyComments input inputFields output = do
    outputFields <- parseFields PanicCannotParseOutput (C.toUTF8BS output)
    indentWith <- asks optIndent

    let comments      = extractComments input
        inputFieldsU  = fieldUniverseN inputFields
        outputFieldsU = fieldUniverseN outputFields

        addComment'   = addComment indentWith inputFieldsU outputFieldsU

        outputLines, outputLines' :: [String]
        outputLines  = lines output
        outputLines' = foldr addComment' outputLines comments

    return $ unlines outputLines'
  where
    insertCommentAt :: Int -> [BS8.ByteString] -> (Int, FieldPath) -> [String] -> [String]
    insertCommentAt indentWith bss (l, fp) xs =
        ys ++ lastEmpty ys ++ map f bss ++ zs
      where
        indent = max 0 (fieldPathSize fp - 1)
        ~(ys, zs) = splitAt (l - 1) xs
        f bs = replicate (indentWith * indent) ' ' ++ C.fromUTF8BS bs

        lastEmpty []       = []
        lastEmpty [[]]     = []
        lastEmpty [_]      = [[]]
        lastEmpty (_ : vs) = lastEmpty vs

    addComment
        :: Int
        -> [(FieldPath, C.Field C.Position)]
        -> [(FieldPath, C.Field C.Position)]
        -> (Int, [BS8.ByteString]) -> [String] -> [String]
    addComment indentWith inputFieldsU outputFieldsU (l, bss) =
        maybe id (insertCommentAt indentWith bss) $ do
            ipath  <- findPath C.fieldAnn l inputFieldsU
            ofield <- lookup ipath outputFieldsU
            let C.Position l' _ = C.fieldAnn ofield
            return (l', ipath)

extractComments :: BS.ByteString -> [(Int, [BS.ByteString])]
extractComments = go . zip [1..] . map (BS.dropWhile isSpace8) . BS8.lines where
    go :: [(Int, BS.ByteString)] -> [(Int, [BS.ByteString])]
    go [] = []
    go ((n, bs) : rest)
        | isComment bs = case span ((isComment .|| BS.null) . snd) rest of
            (h,t) -> (n, bs : map snd h) : go t
        | otherwise = go rest

    (f .|| g) x = f x || g x

    isSpace8 w = w == 9 || w == 32

    isComment :: BS.ByteString -> Bool
    isComment = BS.isPrefixOf "--"

-- | Paths input paths. Essentially a list of offsets. Own type ofr safety.
data FieldPath
    = End
    | Nth Int FieldPath -- nth field
  deriving (Eq, Show)

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

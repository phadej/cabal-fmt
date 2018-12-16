{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
-- | This is a demo application of how you can make Cabal-like
-- file formatter.
--
module Main where

import Control.Arrow               ((&&&))
import Control.Monad               (join)
import Data.Char                   (toLower)
import Data.Functor.Identity       (Identity (..))
import Data.List                   (sortOn)
import Distribution.Compat.Newtype
import System.Environment          (getArgs)

import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Char8                        as BS8
import qualified Data.Map.Strict                              as Map
import qualified Distribution.FieldGrammar                    as C
import qualified Distribution.FieldGrammar.Parsec             as C
import qualified Distribution.Fields                          as C
import qualified Distribution.Fields.Field                    as C
import qualified Distribution.Fields.Pretty                   as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Parsec.Newtypes                 as C
import qualified Distribution.Pretty                          as C
import qualified Distribution.Simple.Utils                    as C
                 (fromUTF8BS, toUTF8BS)
import qualified Distribution.Types.Dependency                as C
import qualified Distribution.Types.DependencyMap             as C
import qualified Distribution.Types.PackageName               as C
import qualified Distribution.Types.VersionInterval           as C
import qualified Distribution.Types.VersionRange              as C
import qualified Language.Haskell.Extension                   as C
import qualified Text.PrettyPrint                             as PP

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    filepath : _ <- getArgs
    input <- BS.readFile filepath

    let indentWith = 2 :: Int

    inputFields <- either (fail . show) pure $ C.readFields input

    let outputPrettyFields = runIdentity $ C.genericFromParsecFields
            (\fn fls ->     Identity $ prettyFieldLines fn fls)
            -- todo: default prettySectionArgs aren't as pretty as they could be
            (\fn secArgs -> Identity $ C.prettySectionArgs fn secArgs)
            inputFields
    let output' = C.showFields' indentWith outputPrettyFields

    outputFields <- either (fail . show) pure $ C.readFields (C.toUTF8BS output')

    let comments      = extractComments input
    let inputFieldsU  = fieldUniverseN inputFields
    let outputFieldsU = fieldUniverseN outputFields

    let insertCommentAt :: [BS8.ByteString] -> (Int, FieldPath) -> [String] -> [String]
        insertCommentAt bss (l, fp) xs =
            ys ++ lastEmpty ys ++ map f bss ++ zs
          where
            indent = max 0 (fieldPathSize fp - 1)
            ~(ys, zs) = splitAt (l - 1) xs
            f bs = replicate (indentWith * indent) ' ' ++ C.fromUTF8BS bs

            lastEmpty []       = []
            lastEmpty [[]]     = []
            lastEmpty [_]      = [[]]
            lastEmpty (_ : vs) = lastEmpty vs

    let addComment :: (Int, [BS8.ByteString]) -> [String] -> [String]
        addComment (l, bss) = maybe id (insertCommentAt bss) $ do
            ipath  <- findPath C.fieldAnn l inputFieldsU
            ofield <- lookup ipath outputFieldsU
            let C.Position l' _ = C.fieldAnn ofield
            return (l', ipath)

    let outputLines  = lines output'
        outputLines' = foldr addComment outputLines comments
        output       = unlines outputLines'

    putStr output

-------------------------------------------------------------------------------
-- Field prettyfying
-------------------------------------------------------------------------------

prettyFieldLines :: C.FieldName -> [C.FieldLine ann] -> PP.Doc
prettyFieldLines fn fls
    | Just doc <- knownField fn fls = doc
    | otherwise                     = C.prettyFieldLines fn fls

knownField :: C.FieldName -> [C.FieldLine ann] -> Maybe PP.Doc
knownField fn fls = join $ fieldDescrLookup fieldDescrs fn $ \p pp ->
    case C.runParsecParser p "<input>" (C.fieldLinesToStream fls) of
        Right x -> Just ( pp x)
        Left _  -> Nothing

fieldDescrs :: FieldDescrs () ()
fieldDescrs =
    F (Map.fromList
        [ ("build-depends", buildDependsSP)
        , ("other-extensions", extensionsSP)
        ])
    <> coerceFieldDescrs C.packageDescriptionFieldGrammar
    <> coerceFieldDescrs C.buildInfoFieldGrammar

-------------------------------------------------------------------------------
-- Special fields
-------------------------------------------------------------------------------

-- | Assume 2.2 format:
--
-- * Use leading-comma
-- * tabular format (but don't try too hard)
-- * try to find ^>= opportunities
--
buildDependsSP :: SP
buildDependsSP = SP pretty parse where
    parse :: C.CabalParsing m => m [C.Dependency]
    parse = unpack' (C.alaList C.CommaVCat) <$> C.parsec

    pretty :: [C.Dependency] -> PP.Doc
    pretty deps = PP.vcat (map pretty' deps') where
        deps' = sortOn (map toLower . fst)
              $ map (C.unPackageName . C.depPkgName &&& C.depVerRange)
              $ C.fromDepMap . C.toDepMap -- this combines duplicate packages
              $ deps

        width = maximum (0 : map (length . fst) deps') + 1

        -- we assume cabal-version: 2.2 or higher
        pretty' :: (String, C.VersionRange) -> PP.Doc
        pretty' (name, vr)
            | vr == C.anyVersion = PP.text "," PP.<+> PP.text name
            | otherwise =
                PP.text "," PP.<+>
                PP.text (leftpad width name) PP.<+>
                C.pretty (norm vr)

    norm :: C.VersionRange -> C.VersionRange
    norm vr = case C.asVersionIntervals vr of
        []     -> vr
        (i:is) -> foldl C.unionVersionRanges (f i) (map f is)
      where
        f (C.LowerBound l C.InclusiveBound, C.UpperBound u C.ExclusiveBound)
            | u == C.majorUpperBound l = C.majorBoundVersion l
        f i' = C.fromVersionIntervals $ C.mkVersionIntervals [i']

extensionsSP :: SP
extensionsSP = SP pretty parse where
    parse :: C.CabalParsing m => m [C.Extension]
    parse = unpack' (C.alaList' C.FSep C.MQuoted) <$> C.parsec

    pretty :: [C.Extension] -> PP.Doc
    pretty = PP.vcat . map C.pretty . sortOn show

leftpad :: Int -> String -> String
leftpad w s = s ++ replicate (w - length s) ' '

-------------------------------------------------------------------------------
-- FieldDescr variant
-------------------------------------------------------------------------------

-- strict pair
data SP = forall f. SP
    { pPretty :: !(f -> PP.Doc)
    , pParse  :: !(forall m. C.CabalParsing m => m f)
    }

-- | Lookup both pretty-printer and value parser.
--
-- As the value of the field is unknown, we have to work with it universally.
--
-- @since 3.0.0.0
--
fieldDescrLookup
    :: C.CabalParsing m
    => FieldDescrs s a
    -> C.FieldName
    -> (forall f. m f -> (f -> PP.Doc) -> r)
    -> Maybe r
fieldDescrLookup (F m) fn kont = kont' <$> Map.lookup fn m where
    kont' (SP a b) = kont b a

-- | A collection field parsers and pretty-printers.
newtype FieldDescrs s a = F { runF :: Map.Map C.FieldName SP }
  deriving (Functor)

coerceFieldDescrs :: FieldDescrs s a -> FieldDescrs () ()
coerceFieldDescrs (F a) = F a

instance Semigroup (FieldDescrs s a) where
    F a <> F b = F (a <> b)

instance Applicative (FieldDescrs s) where
    pure _  = F mempty
    f <*> x = F (mappend (runF f) (runF x))

singletonF
    :: C.FieldName
    -> (f -> PP.Doc)
    -> (forall m. C.CabalParsing m => m f)
    -> FieldDescrs s a
singletonF fn f g = F $ Map.singleton fn (SP f g)

instance C.FieldGrammar FieldDescrs where
    blurFieldGrammar _ (F m) = F m

    booleanFieldDef fn _ def = singletonF fn f C.parsec where
        f :: Bool -> PP.Doc
        f s | s == def  = PP.empty
            | otherwise = PP.text (show s)

    uniqueFieldAla fn _pack _ =
        singletonF fn (C.pretty . pack' _pack) (unpack' _pack <$> C.parsec)

    optionalFieldAla fn _pack _ =
        singletonF fn (C.pretty . pack' _pack) (unpack' _pack <$> C.parsec)

    optionalFieldDefAla fn _pack _ def =
        singletonF fn f (unpack' _pack <$> C.parsec)
      where
        f s | s == def  = PP.empty
            | otherwise = C.pretty (pack' _pack s)

    monoidalFieldAla fn _pack _ =
        singletonF fn (C.pretty . pack' _pack) (unpack' _pack <$> C.parsec)

    prefixedFields _fnPfx _l = F mempty
    knownField _           = pure ()
    deprecatedSince _  _ x = x
    removedIn _ _ x        = x
    availableSince _ _     = id
    hiddenField _          = F mempty

-------------------------------------------------------------------------------
-- Comments retrofitting
-------------------------------------------------------------------------------

extractComments :: BS.ByteString -> [(Int, [BS.ByteString])]
extractComments = go . zip [1..] . map (BS.dropWhile isSpace8) . BS8.lines where
    go :: [(Int, BS.ByteString)] -> [(Int, [BS.ByteString])]
    go [] = []
    go ((n, bs) : rest)
        | isComment bs = case span ((isComment .&& BS.null) . snd) rest of
            (h,t) -> (n, bs : map snd h) : go t
        | otherwise = go rest

    (f .&& g) x = f x && g x

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

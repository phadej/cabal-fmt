{-# LANGUAGE BangPatterns #-}
-- | This is a variant of "Distribution.Fields.Pretty",
-- but this uses annotated 'PrettyField'.
module CabalFmt.PrettyField (
    -- * Fields
    PrettyField (..),
    showFields,
    genericFromParsecFields,
    ) where

import qualified Data.ByteString            as BS
import qualified Distribution.Fields.Field  as C (FieldName)
import qualified Distribution.Fields.Parser as C
import qualified Distribution.Simple.Utils  as C (fromUTF8BS)
import qualified Text.PrettyPrint           as PP

-------------------------------------------------------------------------------
-- PrettyField
-------------------------------------------------------------------------------

data PrettyField ann
    = PrettyField ann C.FieldName PP.Doc
    | PrettySection ann C.FieldName [PP.Doc] [PrettyField ann]

-------------------------------------------------------------------------------
-- showFields
-------------------------------------------------------------------------------

showFields :: (ann -> [String]) -> Int -> [PrettyField ann] -> String
showFields rann n = unlines . renderFields (Opts rann indent) where
    -- few hardcoded, "unrolled"  variants.
    indent | n == 4    = indent4
           | n == 2    = indent2
           | otherwise = (replicate (max n 1) ' ' ++)

    indent4 :: String -> String
    indent4 [] = []
    indent4 xs = ' ' : ' ' : ' ' : ' ' : xs

    indent2 :: String -> String
    indent2 [] = []
    indent2 xs = ' ' : ' ' : xs

data Opts ann = Opts (ann -> [String]) (String -> String)

renderFields :: Opts ann -> [PrettyField ann] -> [String]
renderFields opts fields = flattenBlocks $ map (renderField opts len) fields
  where
    len = maxNameLength 0 fields

    maxNameLength !acc []                            = acc
    maxNameLength !acc (PrettyField _ name _ : rest) = maxNameLength (max acc (BS.length name)) rest
    maxNameLength !acc (PrettySection {}   : rest)   = maxNameLength acc rest

-- | Block of lines,
data Block = Block Margin Margin [String]

data Margin = Margin | NoMargin
  deriving Eq

-- | Collapse margins, any margin = margin
instance Semigroup Margin where
    NoMargin <> NoMargin = NoMargin
    _        <> _        = Margin

flattenBlocks :: [Block] -> [String]
flattenBlocks = go0 where
    go0 [] = []
    go0 (Block _before after strs : blocks) = strs ++ go after blocks

    go _surr' [] = []
    go  surr' (Block before after strs : blocks) = ins $ strs ++ go after blocks where
        ins | surr' <> before == Margin = ("" :)
            | otherwise                 = id

renderField :: Opts ann -> Int -> PrettyField ann -> Block
renderField (Opts rann indent) fw (PrettyField ann name doc) =
    Block before after $ comments ++ lines'
  where
    comments = rann ann
    before = if null comments then NoMargin else Margin

    (lines', after) = case lines narrow of
        []           -> ([ name' ++ ":" ], NoMargin)
        [singleLine] | length singleLine < 60
                     -> ([ name' ++ ": " ++ replicate (fw - length name') ' ' ++ narrow ], NoMargin)
        _            -> ((name' ++ ":") : map indent (lines (PP.render doc)), Margin)

    name' = C.fromUTF8BS name
    narrow = PP.renderStyle narrowStyle doc

    narrowStyle :: PP.Style
    narrowStyle = PP.style { PP.lineLength = PP.lineLength PP.style - fw }

renderField opts@(Opts rann indent) _ (PrettySection ann name args fields) = Block Margin Margin $
    rann ann
    ++
    [ PP.render $ PP.hsep $ PP.text (C.fromUTF8BS name) : args ]
    ++
    (map indent $ renderFields opts fields)

-------------------------------------------------------------------------------
-- Transform from Parsec.Field
-------------------------------------------------------------------------------

genericFromParsecFields
    :: Applicative f
    => (C.FieldName -> [C.FieldLine ann] -> f PP.Doc)     -- ^ transform field contents
    -> (C.FieldName -> [C.SectionArg ann] -> f [PP.Doc])  -- ^ transform section arguments
    -> [C.Field ann]
    -> f [PrettyField ann]
genericFromParsecFields f g = goMany where
    goMany = traverse go

    go (C.Field (C.Name ann name) fls)          = PrettyField ann name <$> f name fls
    go (C.Section (C.Name ann name) secargs fs) = PrettySection ann name <$> g name secargs <*> goMany fs

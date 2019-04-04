{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module CabalFmt.Fields (
    FieldDescrs,
    fieldDescrLookup,
    coerceFieldDescrs,
    singletonF,
    ) where

import Distribution.Compat.Newtype

import qualified Data.Map.Strict           as Map
import qualified Distribution.FieldGrammar as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec       as C
import qualified Distribution.Pretty       as C
import qualified Distribution.Compat.CharParsing as C
import qualified Text.PrettyPrint          as PP

-------------------------------------------------------------------------------
-- FieldDescr variant
-------------------------------------------------------------------------------

-- strict pair
data SP = forall f. SP
    { _pPretty :: !(f -> PP.Doc)
    , _pParse  :: !(forall m. C.CabalParsing m => m f)
    }

-- | Lookup both pretty-printer and value parser.
--
-- As the value of the field is unknown, we have to work with it universally.
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

    booleanFieldDef fn _ _def = singletonF fn f C.parsec where
        f :: Bool -> PP.Doc
        f s = PP.text (show s)

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

    freeTextField fn _ = singletonF fn
        PP.text
        (C.munch $ const True)

    freeTextFieldDef fn _ = singletonF fn
        PP.text
        (C.munch $ const True)

    prefixedFields _fnPfx _l = F mempty
    knownField _           = pure ()
    deprecatedSince _  _ x = x
    removedIn _ _ x        = x
    availableSince _ _     = id
    hiddenField _          = F mempty

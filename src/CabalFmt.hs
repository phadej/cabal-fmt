{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
--
-- This is a demo application of how you can make Cabal-like
-- file formatter.
--
module CabalFmt (cabalFmt) where

import Control.Monad        (foldM, join)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (asks, local)
import Data.Foldable        (traverse_)
import Data.Function        ((&))
import Data.Maybe           (fromMaybe)

import qualified Data.ByteString                              as BS
import qualified Distribution.CabalSpecVersion                as C
import qualified Distribution.FieldGrammar.Parsec             as C
import qualified Distribution.Fields                          as C
import qualified Distribution.Fields.ConfVar                  as C
import qualified Distribution.Fields.Pretty                   as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Pretty                          as C
import qualified Distribution.Simple.Utils                    as C
import qualified Distribution.Types.Condition                 as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Types.Version                   as C
import qualified Text.PrettyPrint                             as PP

import CabalFmt.Comments
import CabalFmt.Fields
import CabalFmt.Fields.BuildDepends
import CabalFmt.Fields.Extensions
import CabalFmt.Fields.Modules
import CabalFmt.Fields.TestedWith
import CabalFmt.Monad
import CabalFmt.Options
import CabalFmt.Parser
import CabalFmt.Pragma
import CabalFmt.Refactoring

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

cabalFmt :: MonadCabalFmt m => FilePath -> BS.ByteString -> m String
cabalFmt filepath contents = do
    gpd          <- parseGpd filepath contents
    inputFields' <- parseFields contents
    let (inputFieldsC, endComments) = attachComments contents inputFields'
    let (ws, pragmas) = foldMap (foldMap parsePragmas) inputFieldsC <> parsePragmas endComments

    traverse_ displayWarning ws

    inputFields  <- foldM (&) inputFieldsC refactorings

    let optsEndo :: OptionsMorphism
        optsEndo = foldMap pragmaToOM pragmas

    let v = C.cabalSpecFromVersionDigits
          $ C.versionNumbers
          $ C.specVersion
          $ C.packageDescription gpd

    local (\o -> runOptionsMorphism optsEndo $ o { optSpecVersion = v }) $ do
        indentWith   <- asks optIndent

        outputPrettyFields <- C.genericFromParsecFields
            prettyFieldLines
            prettySectionArgs
            inputFields

        return $ C.showFields' fromComments indentWith outputPrettyFields
            & if nullComments endComments then id else
                (++ unlines ("" : [ C.fromUTF8BS c | c <- unComments endComments ]))

fromComments :: Comments -> [String]
fromComments (Comments bss) = map C.fromUTF8BS bss

-------------------------------------------------------------------------------
-- Refactorings
-------------------------------------------------------------------------------

refactorings :: MonadCabalFmt m => [Refactoring' m]
refactorings =
    [ refactoringExpandExposedModules
    ]

-------------------------------------------------------------------------------
-- Field prettyfying
-------------------------------------------------------------------------------

prettyFieldLines :: MonadCabalFmt m => C.FieldName -> [C.FieldLine ann] -> m PP.Doc
prettyFieldLines fn fls =
    fromMaybe (C.prettyFieldLines fn fls) <$> knownField fn fls

knownField :: MonadCabalFmt m => C.FieldName -> [C.FieldLine ann] -> m (Maybe PP.Doc)
knownField fn fls = do
    v <- asks optSpecVersion
    return $ join $ fieldDescrLookup (fieldDescrs v) fn $ \p pp ->
        case C.runParsecParser' v p "<input>" (C.fieldLinesToStream fls) of
            Right x -> Just (pp x)
            Left _  -> Nothing

fieldDescrs :: C.CabalSpecVersion -> FieldDescrs () ()
fieldDescrs v
    =  buildDependsF v
    <> setupDependsF v
    <> defaultExtensionsF
    <> otherExtensionsF
    <> exposedModulesF
    <> otherModulesF
    <> testedWithF
    <> coerceFieldDescrs C.packageDescriptionFieldGrammar
    <> coerceFieldDescrs C.buildInfoFieldGrammar

-------------------------------------------------------------------------------
-- Sections
-------------------------------------------------------------------------------

prettySectionArgs :: MonadCabalFmt m => C.FieldName -> [C.SectionArg ann] -> m [PP.Doc]
prettySectionArgs x args =
    prettySectionArgs' x args `catchError` \_ ->
        return (C.prettySectionArgs x args)

prettySectionArgs' :: MonadCabalFmt m => a -> [C.SectionArg ann] -> m [PP.Doc]
prettySectionArgs' _ args = do
    c <- runParseResult "<args>" "" $ C.parseConditionConfVar (map (C.zeroPos <$) args)
    return [ppCondition c]

-------------------------------------------------------------------------------
-- PrettyPrint condition
-------------------------------------------------------------------------------

-- This is originally from Cabal

ppCondition :: C.Condition C.ConfVar -> PP.Doc
ppCondition (C.Var x)      = ppConfVar x
ppCondition (C.Lit b)      = PP.text (show b)
ppCondition (C.CNot c)     = PP.char '!' PP.<> ppCondition c
ppCondition (C.COr c1 c2)  = PP.parens (PP.hsep [ppCondition c1, PP.text "||", ppCondition c2])
ppCondition (C.CAnd c1 c2) = PP.parens (PP.hsep [ppCondition c1, PP.text "&&", ppCondition c2])

ppConfVar :: C.ConfVar -> PP.Doc
ppConfVar (C.OS os)     = PP.text "os"   PP.<> PP.parens (C.pretty os)
ppConfVar (C.Arch arch) = PP.text "arch" PP.<> PP.parens (C.pretty arch)
ppConfVar (C.Flag name) = PP.text "flag" PP.<> PP.parens (C.pretty name)
ppConfVar (C.Impl c v)  = PP.text "impl" PP.<> PP.parens (C.pretty c PP.<+> C.pretty v)

-------------------------------------------------------------------------------
-- Pragma to OM
-------------------------------------------------------------------------------

pragmaToOM :: Pragma -> OptionsMorphism
pragmaToOM (PragmaOptIndent n)    = mkOptionsMorphism $ \opts -> opts { optIndent = n }
pragmaToOM PragmaNoOp             = mempty
pragmaToOM PragmaExpandModules {} = mempty


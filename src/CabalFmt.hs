{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This is a demo application of how you can make Cabal-like
-- file formatter.
--
module CabalFmt where

import Control.Monad        (join)
import Control.Monad.Reader (asks, local)
import Data.Maybe           (fromMaybe)
import System.Environment   (getArgs)
import System.Exit          (exitFailure)

import qualified Data.ByteString                              as BS
import qualified Distribution.CabalSpecVersion                as C
import qualified Distribution.FieldGrammar.Parsec             as C
import qualified Distribution.Fields                          as C
import qualified Distribution.Fields.Pretty                   as C
import qualified Distribution.PackageDescription.FieldGrammar as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Types.Version                   as C
import qualified Text.PrettyPrint                             as PP

import CabalFmt.Comments
import CabalFmt.Error
import CabalFmt.Fields
import CabalFmt.Fields.BuildDepends
import CabalFmt.Fields.Extensions
import CabalFmt.Fields.Modules
import CabalFmt.Monad
import CabalFmt.Options
import CabalFmt.Parser

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        []           -> BS.getContents       >>= main' "<stdin>"
        (filepath:_) -> BS.readFile filepath >>= main' filepath

main' :: FilePath -> BS.ByteString -> IO ()
main' filepath input = case runCabalFmt defaultOptions (main'' filepath input) of
    Right output -> putStr output
    Left err     -> do
        print err
        exitFailure

main'' :: FilePath -> BS.ByteString -> CabalFmt String
main'' filepath contents = do
    indentWith  <- asks optIndent
    gpd         <- parseGpd filepath contents
    inputFields <- parseFields PanicCannotParseInput contents

    let v = C.cabalSpecFromVersionDigits
          $ C.versionNumbers
          $ C.specVersion
          $ C.packageDescription gpd

    local (\opts -> opts { optSpecVersion = v }) $ do

        outputPrettyFields <- C.genericFromParsecFields
            prettyFieldLines
            -- todo: default prettySectionArgs aren't as pretty as they could be
            (\fn secArgs -> return $ C.prettySectionArgs fn secArgs)
            inputFields

        copyComments contents inputFields $ C.showFields' indentWith outputPrettyFields

-------------------------------------------------------------------------------
-- Field prettyfying
-------------------------------------------------------------------------------

prettyFieldLines :: C.FieldName -> [C.FieldLine ann] -> CabalFmt PP.Doc
prettyFieldLines fn fls =
    fromMaybe (C.prettyFieldLines fn fls) <$> knownField fn fls

knownField :: C.FieldName -> [C.FieldLine ann] -> CabalFmt (Maybe PP.Doc)
knownField fn fls = do
    v <- asks optSpecVersion
    return $ join $ fieldDescrLookup (fieldDescrs v) fn $ \p pp ->
        case C.runParsecParser' v p "<input>" (C.fieldLinesToStream fls) of
            Right x -> Just (pp x)
            Left _  -> Nothing

fieldDescrs :: C.CabalSpecVersion -> FieldDescrs () ()
fieldDescrs v
    = buildDependsF v
    <> defaultExtensionsF
    <> otherExtensionsF
    <> exposedModulesF
    <> otherModulesF
    <> coerceFieldDescrs C.packageDescriptionFieldGrammar
    <> coerceFieldDescrs C.buildInfoFieldGrammar

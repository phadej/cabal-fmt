module CabalFmt.Parser where

import Text.Parsec.Error (ParseError)
import Control.Monad.Error (throwError)

import qualified Distribution.Fields       as C
import qualified Distribution.Fields.Field as C
import qualified Distribution.Parsec       as C
import qualified Distribution.PackageDescription.Parsec as C
import qualified Distribution.Types.GenericPackageDescription       as C
import qualified Data.ByteString as BS

import CabalFmt.Monad
import CabalFmt.Error

parseGpd :: FilePath -> BS.ByteString -> CabalFmt C.GenericPackageDescription
parseGpd filepath contents = case result of
    Right gpd -> return gpd
    Left (mspecVersion, errors) -> throwError $ CabalParseError filepath contents errors mspecVersion warnings
  where
    (warnings, result) = C.runParseResult $ C.parseGenericPackageDescription contents

parseFields :: (ParseError -> Error) -> BS.ByteString -> CabalFmt [C.Field C.Position]
parseFields mkErr contents = case C.readFields contents of
    Left err -> throwError $ mkErr err
    Right x  -> return x

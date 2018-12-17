module CabalFmt.Parser where

import Control.Monad.Error (throwError)
import Text.Parsec.Error   (ParseError)

import qualified Data.ByteString                              as BS
import qualified Distribution.Fields                          as C
import qualified Distribution.PackageDescription.Parsec       as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Types.GenericPackageDescription as C

import CabalFmt.Error
import CabalFmt.Monad

runParseResult :: FilePath -> BS.ByteString -> C.ParseResult a -> CabalFmt a
runParseResult filepath contents pr = case result of
    Right gpd -> return gpd
    Left (mspecVersion, errors) -> throwError $ CabalParseError filepath contents errors mspecVersion warnings
  where
    (warnings, result) = C.runParseResult pr

parseGpd :: FilePath -> BS.ByteString -> CabalFmt C.GenericPackageDescription
parseGpd filepath contents = runParseResult filepath contents $ C.parseGenericPackageDescription contents

parseFields :: (ParseError -> Error) -> BS.ByteString -> CabalFmt [C.Field C.Position]
parseFields mkErr contents = case C.readFields contents of
    Left err -> throwError $ mkErr err
    Right x  -> return x

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Parser where

import Control.Monad.Except (throwError)

import qualified Data.ByteString                              as BS
import qualified Distribution.Fields                          as C
import qualified Distribution.PackageDescription.Parsec       as C
import qualified Distribution.Parsec                          as C
import qualified Distribution.Types.GenericPackageDescription as C

import CabalFmt.Error
import CabalFmt.Monad

runParseResult :: MonadCabalFmt m => FilePath -> BS.ByteString -> C.ParseResult a -> m a
runParseResult filepath contents pr = case result of
    Right gpd -> return gpd
    Left (mspecVersion, errors) -> throwError $ CabalParseError filepath contents errors mspecVersion warnings
  where
    (warnings, result) = C.runParseResult pr

parseGpd :: MonadCabalFmt m => FilePath -> BS.ByteString -> m C.GenericPackageDescription
parseGpd filepath contents = runParseResult filepath contents $ C.parseGenericPackageDescription contents

parseFields :: MonadCabalFmt m => BS.ByteString -> m [C.Field C.Position]
parseFields contents = case C.readFields contents of
    Left err -> throwError $ PanicCannotParseInput err
    Right x  -> return x

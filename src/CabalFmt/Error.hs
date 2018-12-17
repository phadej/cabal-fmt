module CabalFmt.Error where

import Text.Parsec.Error (ParseError)

import qualified Data.ByteString            as BS
import qualified Distribution.Parsec        as C
import qualified Distribution.Types.Version as C

data Error
    = SomeError String
    | CabalParseError FilePath BS.ByteString [C.PError] (Maybe C.Version) [C.PWarning]
    | PanicCannotParseInput  ParseError
  deriving (Show)

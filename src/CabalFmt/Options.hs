-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Options (
    Options (..),
    defaultOptions,
    parseOptionsMorphism,
    OptionsMorphism, runOptionsMorphism,
    ) where

import Data.ByteString (ByteString)

import qualified Distribution.CabalSpecVersion       as C
import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C

import Debug.Trace

data Options = Options
    { optIndent      :: !Int
    , optSpecVersion :: !C.CabalSpecVersion
    }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optIndent      = 2
    , optSpecVersion = C.cabalSpecLatest
    }

newtype OptionsMorphism = OM (Options -> Options)

runOptionsMorphism :: OptionsMorphism -> Options -> Options
runOptionsMorphism (OM f) = f

instance Semigroup OptionsMorphism where
    OM f <> OM g = OM (g . f)

instance Monoid OptionsMorphism where
    mempty  = OM id
    mappend = (<>)

parseOptionsMorphism :: ByteString -> OptionsMorphism
parseOptionsMorphism bs
    = either (const mempty ) id
    $ C.runParsecParser parser "<input>" $ C.fieldLineStreamFromBS bs
  where
    parser :: C.ParsecParser OptionsMorphism
    parser = do
        _ <- C.string "--"
        C.spaces
        _ <- C.string "cabal-fmt:"
        fs <- flip C.sepBy C.space $ do
            C.spaces
            indent
        return $ mconcat fs

    indent :: C.ParsecParser OptionsMorphism
    indent = do
        n <- C.string "indent" *> C.char '=' *> C.integral
        return $ OM $ \opts -> opts { optIndent = n}

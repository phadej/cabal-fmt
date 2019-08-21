-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Options (
    Options (..),
    defaultOptions,
    OptionsMorphism, mkOptionsMorphism, runOptionsMorphism,
    ) where

import Data.ByteString (ByteString)

import qualified Distribution.CabalSpecVersion       as C
import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C

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

mkOptionsMorphism :: (Options -> Options) -> OptionsMorphism
mkOptionsMorphism = OM

instance Semigroup OptionsMorphism where
    OM f <> OM g = OM (g . f)

instance Monoid OptionsMorphism where
    mempty  = OM id
    mappend = (<>)


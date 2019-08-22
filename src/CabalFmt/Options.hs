-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Options (
    Options (..),
    defaultOptions,
    OptionsMorphism, mkOptionsMorphism, runOptionsMorphism,
    ) where

import qualified Distribution.CabalSpecVersion       as C

data Options = Options
    { optError       :: !Bool
    , optIndent      :: !Int
    , optTabular     :: !Bool
    , optSpecVersion :: !C.CabalSpecVersion
    }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optError       = False
    , optIndent      = 2
    , optTabular     = True
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


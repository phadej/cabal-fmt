{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module CabalFmt.Monad (
    CabalFmt,
    runCabalFmt,
    ) where

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)

import CabalFmt.Error
import CabalFmt.Options

newtype CabalFmt a = CabalFmt { unCabalFmt :: ReaderT Options (Either Error) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Options, MonadError Error)

runCabalFmt :: Options -> CabalFmt a -> Either Error a
runCabalFmt opts m = runReaderT (unCabalFmt m) opts

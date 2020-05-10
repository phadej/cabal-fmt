-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative (many, (<**>))
import Data.Foldable       (asum, for_)
import Data.Maybe          (fromMaybe)
import Data.Version        (showVersion)
import System.Exit         (exitFailure)
import System.FilePath     (takeDirectory)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as O

import CabalFmt         (cabalFmt)
import CabalFmt.Error   (renderError)
import CabalFmt.Monad   (runCabalFmtIO)
import CabalFmt.Options
import CabalFmt.Prelude

import Paths_cabal_fmt (version)

main :: IO ()
main = do
    (inplace, opts', filepaths) <- O.execParser optsP'
    let opts = runOptionsMorphism opts' defaultOptions

    case filepaths of
        []    -> BS.getContents >>= main' False opts Nothing
        (_:_) -> for_ filepaths $ \filepath -> do
            contents <- BS.readFile filepath
            main' inplace opts (Just filepath) contents
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Reformat .cabal files"
        , O.header "cabal-fmt - .cabal file reformatter"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

main' :: Bool -> Options -> Maybe FilePath -> BS.ByteString -> IO ()
main' inplace opts mfilepath input = do
    -- name of the input
    let filepath = fromMaybe "<stdin>" mfilepath

    -- process
    res <- runCabalFmtIO (takeDirectory <$> mfilepath) opts (cabalFmt filepath input)

    case res of
        Right output
            | inplace   -> BS.writeFile filepath outputBS
            | otherwise -> BS.putStr outputBS
          where outputBS = toUTF8BS output
        Left err     -> do
            renderError err
            exitFailure

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

optsP :: O.Parser (Bool, OptionsMorphism, [FilePath])
optsP = (,,)
    <$> O.flag False True (O.short 'i' <> O.long "inplace" <> O.help "process files in-place")
    <*> optsP'
    <*> many (O.strArgument (O.metavar "FILE..." <> O.help "input files"))
  where
    optsP' = fmap mconcat $ many $ asum
        [ werrorP
        , noWerrorP
        , indentP
        , tabularP
        , noTabularP
        ]

    werrorP = O.flag' (mkOptionsMorphism $ \opts -> opts { optError = True })
        $ O.long "Werror" <> O.help "Treat warnings as errors"

    noWerrorP = O.flag' (mkOptionsMorphism $ \opts -> opts { optError = False })
        $ O.long "Wno-error"

    indentP = O.option (fmap (\n -> mkOptionsMorphism $ \opts -> opts { optIndent = n}) O.auto)
        $ O.long "indent" <> O.help "Indentation" <> O.metavar "N"

    tabularP = O.flag' (mkOptionsMorphism $ \opts -> opts { optTabular = True })
        $ O.long "tabular" <> O.help "Tabular formatting"

    noTabularP = O.flag' (mkOptionsMorphism $ \opts -> opts { optTabular = False })
        $ O.long "no-tabular"


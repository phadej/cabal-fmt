-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative (many, (<**>))
import Control.Monad       (unless, when)
import Data.Foldable       (asum, for_)
import Data.Traversable    (for)
import Data.Version        (showVersion)
import System.Exit         (exitFailure)
import System.FilePath     (takeDirectory)
import System.IO           (hPutStrLn, stderr)

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
    (opts', filepaths) <- O.execParser optsP'
    let opts = runOptionsMorphism opts' defaultOptions

    notFormatted <- catMaybes <$> case filepaths of
        []    -> fmap pure $ BS.getContents >>= main' opts Nothing
        (_:_) -> for filepaths $ \filepath -> do
            contents <- BS.readFile filepath
            main' opts (Just filepath) contents

    when ((optMode opts == ModeCheck) && not (null notFormatted)) $ do
        for_ notFormatted $ \filepath ->
            hPutStrLn stderr $ "error: Input " <> filepath <> " is not formatted."
        exitFailure

  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Reformat .cabal files"
        , O.header "cabal-fmt - .cabal file reformatter"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

main' :: Options -> Maybe FilePath -> BS.ByteString -> IO (Maybe FilePath)
main' opts mfilepath input = do
    -- name of the input
    let filepath = fromMaybe "<stdin>" mfilepath

    -- process
    res <- runCabalFmtIO (takeDirectory <$> mfilepath) opts (cabalFmt filepath input)

    case res of
        Right output -> do
            let outputBS = toUTF8BS output
                formatted = outputBS == input

            case optMode opts of
                ModeStdout -> BS.putStr outputBS
                ModeInplace -> case mfilepath of
                    Nothing -> BS.putStr outputBS
                    Just _  -> unless formatted $ BS.writeFile filepath outputBS
                _ -> return ()

            return $ if formatted then Nothing else Just filepath

        Left err     -> do
            renderError err
            exitFailure

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

optsP :: O.Parser (OptionsMorphism, [FilePath])
optsP = (,)
    <$> optsP'
    <*> many (O.strArgument (O.metavar "FILE..." <> O.help "input files"))
  where
    optsP' = fmap mconcat $ many $ asum
        [ werrorP
        , noWerrorP
        , indentP
        , tabularP
        , noTabularP
        , stdoutP
        , inplaceP
        , checkP
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

    stdoutP = O.flag' (mkOptionsMorphism $ \opts -> opts { optMode = ModeStdout })
        $ O.long "stdout" <> O.help "Write output to stdout (default)"

    inplaceP = O.flag' (mkOptionsMorphism $ \opts -> opts { optMode = ModeInplace })
        $ O.short 'i' <> O.long "inplace" <> O.help "Process files in-place"

    checkP = O.flag' (mkOptionsMorphism $ \opts -> opts { optMode = ModeCheck })
        $ O.short 'c' <> O.long "check" <> O.help "Fail with non-zero exit code if input is not formatted"


-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative (many, (<**>))
import Data.Foldable       (asum, for_)
import System.Exit         (exitFailure)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as O

import CabalFmt         (cabalFmt)
import CabalFmt.Error   (renderError)
import CabalFmt.Monad   (runCabalFmtIO)
import CabalFmt.Options

main :: IO ()
main = do
    (inplace, opts', filepaths) <- O.execParser optsP'
    let opts = runOptionsMorphism opts' defaultOptions

    case filepaths of
        []    -> BS.getContents >>= main' False opts "<stdin>"
        (_:_) -> for_ filepaths $ \filepath ->
            BS.readFile filepath >>= main' inplace opts filepath
  where
    optsP' = O.info (optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "Reformat .cabal files"
        , O.header "cabal-fmt - .cabal file reformatter"
        ]

main' :: Bool -> Options -> FilePath -> BS.ByteString -> IO ()
main' inplace opts filepath input = do
    res <- runCabalFmtIO opts (cabalFmt filepath input)
    case res of
        Right output
            | inplace   -> writeFile filepath output
            | otherwise -> putStr output
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
        ]
        -- <$> werrorP
        -- <*> O.option O.auto (O.long "indent" <> O.value (optIndent defaultOptions) <> O.help "Indentation" <> O.showDefault)
        -- <*> pure (optSpecVersion defaultOptions)

    werrorP = O.flag' (mkOptionsMorphism $ \opts -> opts { optError = True })
        $ O.long "Werror" <> O.help "Treat warnings as errors"

    noWerrorP = O.flag' (mkOptionsMorphism $ \opts -> opts { optError = False })
        $ O.long "Wno-error" <> O.help "Treat warnings as warnings"

    indentP = O.option (fmap (\n -> mkOptionsMorphism $ \opts -> opts { optIndent = n}) O.auto)
        $ O.long "indent" <> O.help "Indentation" <> O.metavar "N"

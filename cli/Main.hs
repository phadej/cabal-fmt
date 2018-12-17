module Main (main) where

import Control.Applicative (many, (<**>))
import Data.Foldable       (for_)
import System.Exit         (exitFailure)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as O

import CabalFmt         (cabalFmt)
import CabalFmt.Monad   (runCabalFmt)
import CabalFmt.Options
import CabalFmt.Error (renderError)

main :: IO ()
main = do
    (inplace, opts, filepaths) <- O.execParser optsP'
    case filepaths of
        []    -> BS.getContents       >>= main' False opts "<stdin>"
        (_:_) -> for_ filepaths $ \filepath ->
            BS.readFile filepath >>= main' inplace opts filepath
  where
    optsP' = O.info (optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "Reformat .cabal files"
        , O.header "cabal-fmt - .cabal file reformatter"
        ]

main' :: Bool -> Options -> FilePath -> BS.ByteString -> IO ()
main' inplace opts filepath input =
    case runCabalFmt opts (cabalFmt filepath input) of
        Right output
            | inplace   -> writeFile filepath output
            | otherwise -> putStr output
        Left err     -> do
            renderError err
            exitFailure

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

optsP :: O.Parser (Bool, Options, [FilePath])
optsP = (,,)
    <$> O.flag False True (O.short 'i' <> O.long "inplace" <> O.help "process files in-place")
    <*> optsP'
    <*> many (O.strArgument (O.metavar "FILE" <> O.help "input files"))
  where
    optsP' = Options
        <$> O.option O.auto (O.long "indent" <> O.value (optIndent defaultOptions) <> O.help "Indentation" <> O.showDefault)
        <*> pure (optSpecVersion defaultOptions)

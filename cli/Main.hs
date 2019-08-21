-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative (many, (<**>))
import Data.Foldable       (for_)
import System.Directory    (doesDirectoryExist, getDirectoryContents)
import System.Exit         (exitFailure)
import System.FilePath     (takeDirectory, (</>))
import System.IO.Unsafe    (unsafeInterleaveIO)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as O

import CabalFmt         (cabalFmt)
import CabalFmt.Error   (renderError)
import CabalFmt.Monad   (runCabalFmt)
import CabalFmt.Options

main :: IO ()
main = do
    (inplace, opts', filepaths) <- O.execParser optsP'

    -- glob all files, only when a single filepath is given.
    files <- case filepaths of
        [fp] -> getFiles (takeDirectory fp)
        _    -> return []
    let opts = opts' { optFileList = files }

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
    <*> many (O.strArgument (O.metavar "FILE..." <> O.help "input files"))
  where
    optsP' = Options
        <$> O.option O.auto (O.long "indent" <> O.value (optIndent defaultOptions) <> O.help "Indentation" <> O.showDefault)
        <*> pure (optSpecVersion defaultOptions)
        <*> pure []

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

getFiles :: FilePath -> IO [FilePath]
getFiles = getDirectoryContentsRecursive' check where
    check "dist-newstyle" = False
    check ('.' : _)       = False
    check _               = True

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is used.
--
-- /Note:/ From @Cabal@'s "Distribution.Simple.Utils"
getDirectoryContentsRecursive'
    :: (FilePath -> Bool) -- ^ Check, whether to recurse
    -> FilePath           -- ^ top dir
    -> IO [FilePath]
getDirectoryContentsRecursive' ignore' topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories []         = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')

      where
        collect files dirs' []              = return (reverse files
                                                     ,reverse dirs')
        collect files dirs' (entry:entries) | ignore entry
                                            = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries

        ignore ['.']      = True
        ignore ['.', '.'] = True
        ignore x          = not (ignore' x)

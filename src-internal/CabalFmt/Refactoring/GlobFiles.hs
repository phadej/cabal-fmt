-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalFmt.Refactoring.GlobFiles (
    refactoringGlobFiles,
) where

import qualified Distribution.Fields   as C
import qualified System.FilePath       as Native
import qualified System.FilePath.Posix as Posix

import CabalFmt.Glob
import CabalFmt.Monad
import CabalFmt.Pragma
import CabalFmt.Prelude
import CabalFmt.Refactoring.Type

refactoringGlobFiles :: FieldRefactoring
refactoringGlobFiles C.Section {} = pure Nothing
refactoringGlobFiles (C.Field name@(C.Name (_, pragmas) _n) fls) = do
    globs <- parse pragmas
    files <- fmap concat (traverse match' globs)

    let newFiles :: [C.FieldLine CommentsPragmas]
        newFiles = catMaybes
            [ return $ C.FieldLine mempty $ toUTF8BS file
            | file <- files
            ]

    pure $ case files of
        [] -> Nothing
        _  -> Just (C.Field name (newFiles ++ fls))

  where
    parse :: MonadCabalFmt r m => [FieldPragma] -> m [Glob]
    parse = fmap mconcat . traverse go where
        go (PragmaGlobFiles g) = return [ g ]
        go p = do
            displayWarning $ "Skipped pragma " ++ show p
            return []

    match' :: MonadCabalFmt r m => Glob -> m [FilePath]
    match' g@(Glob dir _) = do
        files <- map (\fp -> dir Native.</> fp) <$> getFiles dir
        return $ map toPosix $ filter (match g) files

    toPosix :: FilePath -> FilePath
    toPosix = Posix.joinPath . Native.splitDirectories

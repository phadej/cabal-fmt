-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module CabalFmt.Refactoring.ExpandExposedModules (
    refactoringExpandExposedModules,
    ) where

import qualified Distribution.Fields     as C
import qualified Distribution.ModuleName as C

import CabalFmt.Prelude
import CabalFmt.Monad
import CabalFmt.Pragma
import CabalFmt.Refactoring.Type

refactoringExpandExposedModules :: FieldRefactoring
refactoringExpandExposedModules C.Section {} = pure Nothing
refactoringExpandExposedModules (C.Field name@(C.Name (_, _, pragmas) _n) fls) = do
    dirs <- parse pragmas
    files <- traverseOf (traverse . _1) getFiles dirs

    let newModules :: [C.FieldLine CommentsPragmas]
        newModules = catMaybes
            [ return $ C.FieldLine emptyCommentsPragmas $ toUTF8BS $ intercalate "." parts
            | (files', mns) <- files
            , file <- files'
            , let parts = splitDirectories $ dropExtension file
            , all C.validModuleComponent parts
            , let mn = C.fromComponents parts -- TODO: don't use fromComponents
            , mn `notElem` mns
            ]

    pure $ case newModules of
        [] -> Nothing
        _  -> Just (C.Field name (newModules ++ fls))

  where
    parse :: MonadCabalFmt r m => [FieldPragma] -> m [(FilePath, [C.ModuleName])]
    parse = fmap mconcat . traverse go where
        go (PragmaExpandModules fp mns) = return [ (fp, mns) ]
        go p = do
            displayWarning $ "Skipped pragma " ++ show p
            return []

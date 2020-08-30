-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalFmt.Refactoring.ExpandExposedModules (
    refactoringExpandExposedModules,
    ) where

import qualified Distribution.Fields     as C
import qualified Distribution.ModuleName as C

import CabalFmt.Prelude
import CabalFmt.Monad
import CabalFmt.Pragma
import CabalFmt.Refactoring.Type

refactoringExpandExposedModules :: Refactoring
refactoringExpandExposedModules = traverseFields refact where
    refact :: RefactoringOfField
    refact name@(C.Name (_, pragmas) n) fls
        | n == "exposed-modules" || n == "other-modules" = do
            dirs <- parse pragmas
            files <- traverseOf (traverse . _1) getFiles dirs

            let newModules :: [C.FieldLine CommentsPragmas]
                newModules = catMaybes
                    [ return $ C.FieldLine mempty $ toUTF8BS $ intercalate "." parts
                    | (files', mns) <- files
                    , file <- files'
                    , let parts = splitDirectories $ dropExtension file
                    , all C.validModuleComponent parts
                    , let mn = C.fromComponents parts
                    , mn `notElem` mns
                    ]

            pure (name, newModules ++ fls)
        | otherwise = pure (name, fls)

    parse :: MonadCabalFmt r m => [Pragma] -> m [(FilePath, [C.ModuleName])]
    parse = fmap mconcat . traverse go where
        go (PragmaExpandModules fp mns) = return [ (fp, mns) ]
        go p = do
            displayWarning $ "Skipped pragma " ++ show p
            return []



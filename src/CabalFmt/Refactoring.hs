-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalFmt.Refactoring (
    Refactoring,
    Refactoring',
    refactoringExpandExposedModules,
    ) where

import Data.List       (intercalate)
import Data.Maybe      (catMaybes)
import System.FilePath (dropExtension, splitDirectories)

import qualified Distribution.Fields       as C
import qualified Distribution.ModuleName   as C
import qualified Distribution.Simple.Utils as C

import CabalFmt.Comments
import CabalFmt.Monad
import CabalFmt.Pragma

-------------------------------------------------------------------------------
-- Refactoring type
-------------------------------------------------------------------------------

type C = (Comments, [Pragma])
type Refactoring             = forall r m. MonadCabalFmt r m => Refactoring' r m
type Refactoring' r m        = [C.Field C] -> m [C.Field C]
type RefactoringOfField      = forall r m. MonadCabalFmt r m => RefactoringOfField' r m
type RefactoringOfField' r m = C.Name C -> [C.FieldLine C] -> m (C.Name C, [C.FieldLine C])

-------------------------------------------------------------------------------
-- Expand exposed-modules
-------------------------------------------------------------------------------

refactoringExpandExposedModules :: Refactoring
refactoringExpandExposedModules = traverseFields refact where
    refact :: RefactoringOfField
    refact name@(C.Name (_, pragmas) n) fls
        | n == "exposed-modules" || n == "other-modules" = do
            dirs <- parse pragmas
            files <- traverseOf (traverse . _1) getFiles dirs

            let newModules :: [C.FieldLine C]
                newModules = catMaybes
                    [ return $ C.FieldLine mempty $ C.toUTF8BS $ intercalate "." parts
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

-------------------------------------------------------------------------------
-- Tools
-------------------------------------------------------------------------------

traverseOf
    :: Applicative f
    => ((a -> f b) -> s ->  f t)
    -> (a -> f b) -> s ->  f t
traverseOf = id

_1 :: Functor f => (a -> f b) -> (a, c) -> f (b, c)
_1 f (a, c) = (\b -> (b, c)) <$> f a

traverseFields
    :: Applicative f
    => RefactoringOfField' r f
    -> [C.Field C] -> f [C.Field C]
traverseFields f = goMany where
    goMany = traverse go

    go (C.Field name fls)       = uncurry C.Field <$> f name fls
    go (C.Section name args fs) = C.Section name args <$> goMany fs

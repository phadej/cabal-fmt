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

type Refactoring           = forall m. MonadCabalFmt m => Refactoring' m
type Refactoring' m        = [C.Field Comments] -> m [C.Field Comments]
type RefactoringOfField    = forall m. MonadCabalFmt m => RefactoringOfField' m
type RefactoringOfField' m = C.Name Comments -> [C.FieldLine Comments] -> m (C.Name Comments, [C.FieldLine Comments])

-------------------------------------------------------------------------------
-- Expand exposed-modules
-------------------------------------------------------------------------------

refactoringExpandExposedModules :: Refactoring
refactoringExpandExposedModules = traverseFields refact where
    refact :: RefactoringOfField
    refact name@(C.Name c n) fls
        | n == "exposed-modules" || n == "other-modules"
        , dirs <- parse c = do
            files <- traverseOf (traverse . _1) getFiles dirs

            let newModules :: [C.FieldLine Comments]
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

    parse :: Comments -> [(FilePath, [C.ModuleName])]
    parse c = case parsePragmas c of
        (_, pragmas) ->
            [ (fp, mns)
            | PragmaExpandModules fp mns <- pragmas
            ]

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
    => RefactoringOfField' f
    -> [C.Field Comments] -> f [C.Field Comments]
traverseFields f = goMany where
    goMany = traverse go

    go (C.Field name fls)       = uncurry C.Field <$> f name fls
    go (C.Section name args fs) = C.Section name args <$> goMany fs

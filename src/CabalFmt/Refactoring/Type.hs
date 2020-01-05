-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalFmt.Refactoring.Type (
    Refactoring,
    Refactoring',
    RefactoringOfField,
    RefactoringOfField',
    CommentsPragmas,
    traverseFields
    ) where

import qualified Distribution.Fields       as C

import CabalFmt.Comments
import CabalFmt.Monad
import CabalFmt.Pragma

-------------------------------------------------------------------------------
-- Refactoring type
-------------------------------------------------------------------------------

type CommentsPragmas = (Comments, [Pragma])
type Refactoring             = forall r m. MonadCabalFmt r m => Refactoring' r m
type Refactoring' r m        = [C.Field CommentsPragmas] -> m [C.Field CommentsPragmas]
type RefactoringOfField      = forall r m. MonadCabalFmt r m => RefactoringOfField' r m
type RefactoringOfField' r m = C.Name CommentsPragmas -> [C.FieldLine CommentsPragmas] -> m (C.Name CommentsPragmas, [C.FieldLine CommentsPragmas])

-------------------------------------------------------------------------------
-- Traversing refactoring
-------------------------------------------------------------------------------

-- | Allows modification of single field 
--
-- E.g. sorting extensions *could* be done as refactoring,
-- though it's currently implemented in special pretty-printer.
traverseFields
    :: Applicative f
    => RefactoringOfField' r f
    -> [C.Field CommentsPragmas] -> f [C.Field CommentsPragmas]
traverseFields f = goMany where
    goMany = traverse go

    go (C.Field name fls)       = uncurry C.Field <$> f name fls
    go (C.Section name args fs) = C.Section name args <$> goMany fs

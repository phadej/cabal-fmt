{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Refactoring (
    CommentsPragmas,
    refactor,
    ) where

import qualified Distribution.Fields       as C

import CabalFmt.Refactoring.ExpandExposedModules
import CabalFmt.Refactoring.Fragments
import CabalFmt.Refactoring.Type
import CabalFmt.Monad

-------------------------------------------------------------------------------
-- Refactorings
-------------------------------------------------------------------------------

refactor :: forall m r. MonadCabalFmt r m => [C.Field CommentsPragmas] -> m [C.Field CommentsPragmas]
refactor = rewriteFields rewrite
  where
    rewrite :: C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))
    rewrite f@(C.Field (C.Name _ n) _)
        | n == "exposed-modules" || n == "other-modules" = combine
            [ refactoringFragments
            , refactoringExpandExposedModules
            ] f
        | otherwise = combine
            [ refactoringFragments
            ] f

    rewrite f@(C.Section _ _ _)
        | otherwise = combine
            [ refactoringFragments
            ] f

-- | Try refactorings in turn,
-- considering it done if one applies.
combine
    :: Monad m
    => [C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))]
    -> C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))
combine []     _ = return Nothing
combine (r:rs) f = do
    m <- r f
    case m of
        Nothing -> combine rs f
        Just f' -> return (Just f')

-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CabalFmt.Refactoring.Fragments (
    refactoringFragments,
    ) where

import Text.PrettyPrint (hsep, render)

import qualified Distribution.Fields        as C
import qualified Distribution.Fields.Field  as C
import qualified Distribution.Fields.Pretty as C

import CabalFmt.Comments
import CabalFmt.Monad
import CabalFmt.Parser
import CabalFmt.Pragma
import CabalFmt.Prelude
import CabalFmt.Refactoring.Type

refactoringFragments :: Refactoring
refactoringFragments = rewriteFields refact where
    refact :: MonadCabalFmt r m => C.Field CommentsPragmas -> m (Maybe (C.Field CommentsPragmas))
    refact field = do
        parse (getPragmas field) >>= \mp -> case mp of
            Nothing -> pure Nothing
            Just p  -> readFileBS p >>= \mcontents -> case mcontents of
                NoIO -> pure Nothing
                IOError err -> do
                    displayWarning $ "Fragment " ++ p ++ " failed to read: " ++ show err
                    pure Nothing
                Contents c  -> do
                    fields <- parseFields c
                    case (field, fields) of
                        (_, []) -> do
                            displayWarning $ "Fragment " ++ p ++ " is empty."
                            pure Nothing

                        (C.Field (C.Name _ n) _, C.Section name@(C.Name _ _) arg _ : _) -> do
                            displayWarning $ "Fragment " ++ p ++ " contains a section " ++ showSection name arg ++ ", expecting field " ++ show n ++ "."
                            pure Nothing
                        (C.Section name@(C.Name _ _) arg _, C.Field (C.Name _ n') _ : _) -> do
                            displayWarning $ "Fragment " ++ p ++ " contains a field " ++ show n' ++ ", expection section " ++ showSection name arg ++ "."
                            pure Nothing

                        (C.Field name@(C.Name _ n) _, C.Field (C.Name _ n') fls' : rest) -> do
                            unless (null rest) $
                                displayWarning $ "Fragment " ++ p ++ " contains multiple fields or sections, using only the first."
                            if n == n'
                            then do
                                -- everything is fine, replace
                                pure (Just (C.Field name (noCommentsPragmas fls')))
                            else do
                                displayWarning $ "Fragment " ++ p ++ " contains field " ++ show n' ++ ", expecting field " ++ show n ++ "."
                                pure Nothing

                        (C.Section name@(C.Name _ _) arg _, C.Section name'@(C.Name _ _) arg' fs' : rest) -> do
                            unless (null rest) $
                                displayWarning $ "Fragment " ++ p ++ " contains multiple fields or sections, using only the first."

                            if (void name == void name' && map void arg == map void arg')
                            then do
                                pure (Just (C.Section name arg (noCommentsPragmas fs')))
                            else do
                                displayWarning $ "Fragment " ++ p ++ " contains a section " ++ showSection name arg ++ ", expection section " ++ showSection name' arg' ++ "."
                                pure Nothing

    noCommentsPragmas :: Functor f => [f ann] -> [f CommentsPragmas]
    noCommentsPragmas = map ((Comments [], []) <$)

    getPragmas :: C.Field CommentsPragmas -> [Pragma]
    getPragmas = snd . C.fieldAnn

    showSection :: C.Name ann -> [C.SectionArg ann] -> String
    showSection (C.Name _ n) []   = show n
    showSection (C.Name _ n) args = show (fromUTF8BS n ++ " " ++ render (hsep (C.prettySectionArgs n args)))

    parse :: MonadCabalFmt r m => [Pragma] -> m (Maybe FilePath)
    parse = fmap asum . traverse go where
        go (PragmaFragment f) = return (Just f)
        go _                  = return Nothing

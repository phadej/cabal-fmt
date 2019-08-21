{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Pragma where

import Data.Bifunctor  (first)
import Data.ByteString (ByteString)
import Data.Char       (isLower)
import Data.Either     (partitionEithers)

import qualified Data.ByteString                     as BS
import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.ModuleName             as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C

import CabalFmt.Comments

data Pragma
    = PragmaNoOp
    | PragmaOptIndent Int
    | PragmaExpandModules FilePath [C.ModuleName]
  deriving (Show)

-- | Parse pragma from 'ByteString'.
--
-- An error ('Left') is reported only if input 'ByteString' starts with @-- cabal-fmt:@.
--
parsePragma :: ByteString -> Either String Pragma
parsePragma bs = case dropPrefix bs of
    Nothing  -> Right PragmaNoOp
    Just bs' -> first show $ C.runParsecParser parser "<input>" $ C.fieldLineStreamFromBS bs'
  where
    dropPrefix bs0 = do
        bs1 <- BS.stripPrefix "--" bs0
        bs2 <- BS.stripPrefix "cabal-fmt:" (stripWhitespace bs1)
        return (stripWhitespace bs2)

    parser :: C.ParsecParser Pragma
    parser = do
        t <- C.parsecToken
        case t of
            "expand" -> expandModules
            "indent" -> indent
            _        -> fail $ "Unknown pragma " ++ t

    expandModules :: C.ParsecParser Pragma
    expandModules = do
        C.spaces
        dir <- C.parsecToken
        mns <- C.many (C.space *> C.spaces *> C.char '-' *> C.parsec)
        return (PragmaExpandModules dir mns)

    indent :: C.ParsecParser Pragma
    indent = do
        C.spaces
        n <- C.integral
        return $ PragmaOptIndent n

stripWhitespace :: ByteString -> ByteString
stripWhitespace bs = case BS.uncons bs of
    Nothing                   -> bs
    Just (w, bs') | w == 32   -> stripWhitespace bs'
                  | otherwise -> bs

parsePragmas :: Comments -> ([String], [Pragma])
parsePragmas = partitionEithers . map parsePragma . unComments

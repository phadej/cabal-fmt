module CabalFmt.Glob where

import Data.List             (isInfixOf)
import Data.List.NonEmpty    (NonEmpty (..))

import CabalFmt.Prelude

data Glob = Glob FilePath [GlobPiece]
  deriving Show

data GlobPiece
    = GlobStarStar
    | GlobPiece (NonEmpty GlobChar)
  deriving Show

data GlobChar
    = GlobStar
    | GlobChar Char
  deriving Show

-- | Match glob
--
-- >>> let Right g = parseGlob "cbits/**/*.c"
--
-- >>> map (match g) ["foo", "cbits/header.h", "cbits/source.c", "cbits/dir/source.c"]
-- [False,False,True,True]
--
match :: Glob -> FilePath -> Bool
match (Glob g1 gs0) fp = go0 (splitDirectories fp) where
    go0 []     = False
    go0 (p:ps) = if p == g1 then go ps gs0 else False

    go :: [FilePath] -> [GlobPiece] -> Bool
    go []     []                  = True
    go []     (_:_)               = False
    go (_:_)  []                  = False
    go (s:ss) (GlobStarStar : gs) = go (s:ss) gs || go ss (GlobStarStar : gs)
    go (s:ss) (GlobPiece cs : gs) = matches s (toList cs) && go ss gs


    matches :: FilePath -> [GlobChar] -> Bool
    matches []     []                = True
    matches (_:_)  []                = False
    matches []     (_:_)             = False
    matches (x:xs) (GlobStar : cs)   = matches (x:xs) cs || matches xs (GlobStar : cs)
    matches (x:xs) (GlobChar c : cs) = if x == c then matches xs cs else False

parseGlob :: String -> Either String Glob
parseGlob input = case splitDirectories input of
    []     -> Left "empty path"
    (x:xs) -> do
        p <- parseFirstPiece x
        ps <- traverse parsePiece xs
        return (Glob p ps)
  where
    parseFirstPiece :: String -> Either String FilePath
    parseFirstPiece ""                    = Left "empty path segment"
    parseFirstPiece s | "*" `isInfixOf` s = Left "wild card in first path segment"
    parseFirstPiece s                     = Right s

    parsePiece :: String -> Either String GlobPiece
    parsePiece ""                     = Left "empty path segment"
    parsePiece "**"                   = Right GlobStarStar
    parsePiece s | "**" `isInfixOf` s = Left $ "** inside path segment: " ++ s
    parsePiece (c:cs)                 = Right (GlobPiece (parseChar c :| map parseChar cs))

    parseChar :: Char -> GlobChar
    parseChar '*' = GlobStar
    parseChar c   = GlobChar c

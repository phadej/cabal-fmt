-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module CabalFmt.Error (Error (..), renderError) where

import Control.Exception  (Exception)
import Data.List.NonEmpty (NonEmpty)
import System.FilePath    (normalise)
import System.IO          (hPutStr, hPutStrLn, stderr)
import Text.Parsec.Error  (ParseError)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Distribution.Parsec        as C
import qualified Distribution.Types.Version as C
import qualified Distribution.Utils.Generic as C (fromUTF8BS)

data Error
    = SomeError String
    | CabalParseError FilePath BS.ByteString (NonEmpty C.PError) (Maybe C.Version) [C.PWarning]
    | PanicCannotParseInput  ParseError
    | WarningError String
  deriving (Show)

instance Exception Error

renderError :: Error -> IO ()
renderError (SomeError err) = hPutStrLn stderr $ "error: " ++ err
renderError (PanicCannotParseInput err) = hPutStrLn stderr $ "panic! " ++ show err
renderError (CabalParseError filepath contents errors _ warnings) =
    hPutStr stderr $ renderParseError filepath contents errors warnings
renderError (WarningError w) = hPutStrLn stderr $ "error (-Werror): " ++ w

-------------------------------------------------------------------------------
-- Rendering of Cabal parser error
-------------------------------------------------------------------------------

-- | Render parse error highlighting the part of the input file.
renderParseError
    :: FilePath
    -> BS.ByteString
    -> NonEmpty C.PError
    -> [C.PWarning]
    -> String
renderParseError filepath contents errors warnings = unlines $
    [ "Errors encountered when parsing cabal file " <> filepath <> ":"
    ]
    ++ renderedErrors
    ++ renderedWarnings
  where
    filepath' = normalise filepath

    -- lines of the input file. 'lines' is taken, so they are called rows
    -- contents, line number, whether it's empty line
    rows :: [(String, Int, Bool)]
    rows = zipWith f (BS8.lines contents) [1..] where
        f bs i = let s = C.fromUTF8BS bs in (s, i, isEmptyOrComment s)

    rowsZipper = listToZipper rows

    isEmptyOrComment :: String -> Bool
    isEmptyOrComment s = case dropWhile (== ' ') s of
        ""          -> True   -- empty
        ('-':'-':_) -> True   -- comment
        _           -> False

    renderedErrors   = concatMap renderError' errors
    renderedWarnings = concatMap renderWarning warnings

    renderError' :: C.PError -> [String]
    renderError' (C.PError pos@(C.Position row col) msg)
        -- if position is 0:0, then it doesn't make sense to show input
        -- looks like, Parsec errors have line-feed in them
        | pos == C.zeroPos = msgs
        | otherwise        = msgs ++ formatInput row col
      where
        msgs = [ "", filepath' ++ ":" ++ C.showPos pos ++ ": error:", trimLF msg, "" ]

    renderWarning :: C.PWarning -> [String]
    renderWarning (C.PWarning _ pos@(C.Position row col) msg)
        | pos == C.zeroPos = msgs
        | otherwise        = msgs ++ formatInput row col
      where
        msgs = [ "", filepath' ++ ":" ++ C.showPos pos ++ ": warning:", trimLF msg, "" ]

    -- sometimes there are (especially trailing) newlines.
    trimLF :: String -> String
    trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

    -- format line: prepend the given line number
    formatInput :: Int -> Int -> [String]
    formatInput row col = case advance (row - 1) rowsZipper of
        Zipper xs ys -> before ++ after where
            before = case span (\(_, _, b) -> b) xs of
                (_, [])     -> []
                (zs, z : _) -> map formatInputLine $ z : reverse zs

            after  = case ys of
                []        -> []
                (z : _zs) ->
                    [ formatInputLine z                             -- error line
                    , "      | " ++ replicate (col - 1) ' ' ++ "^"  -- pointer: ^
                    ]
                    -- do we need rows after?
                    -- ++ map formatInputLine (take 1 zs)           -- one row after

    formatInputLine :: (String, Int, Bool) -> String
    formatInputLine (str, row, _) = leftPadShow row ++ " | " ++ str

    -- hopefully we don't need to work with over 99999 lines .cabal files
    -- at that point small glitches in error messages are hopefully fine.
    leftPadShow :: Int -> String
    leftPadShow n = let s = show n in replicate (5 - length s) ' ' ++ s

data Zipper a = Zipper [a] [a]

listToZipper :: [a] -> Zipper a
listToZipper = Zipper []

advance :: Int -> Zipper a -> Zipper a
advance n z@(Zipper xs ys)
    | n <= 0 = z
    | otherwise = case ys of
        []      -> z
        (y:ys') -> advance (n - 1) $ Zipper (y:xs) ys'

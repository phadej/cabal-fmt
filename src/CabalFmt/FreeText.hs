{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.FreeText (
    fieldlinesToFreeText,
    showFreeText,
) where

import Data.List (foldl')

import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Fields.Field     as C
import qualified Distribution.Parsec           as C
import qualified Distribution.Parsec.Position  as C
import qualified Distribution.Pretty           as C
import qualified Distribution.Utils.String     as C (trim)
import qualified Text.PrettyPrint              as PP

import CabalFmt.Prelude

showFreeText :: C.CabalSpecVersion -> String -> PP.Doc
showFreeText v
    | v >= C.CabalSpecV3_0
    = C.showFreeTextV3

    | otherwise
    = C.showFreeText

-- This should perfectly be exported from Cabal-syntax
fieldlinesToFreeText :: C.CabalSpecVersion -> C.Position -> [C.FieldLine C.Position] -> String
fieldlinesToFreeText v
    | v >= C.CabalSpecV3_0
    = fieldlinesToFreeText3

    | otherwise
    = \_ -> fieldlinesToFreeText2

fieldlinesToFreeText2 :: [C.FieldLine C.Position] -> String
fieldlinesToFreeText2 [C.FieldLine _ "."] = "."
fieldlinesToFreeText2 fls = intercalate "\n" (map go fls)
  where
    go (C.FieldLine _ bs)
      | s == "." = ""
      | otherwise = s
      where
        s = C.trim (fromUTF8BS bs)

fieldlinesToFreeText3 :: C.Position -> [C.FieldLine C.Position] -> String
fieldlinesToFreeText3 _ [] = ""
fieldlinesToFreeText3 _ [C.FieldLine _ bs] = fromUTF8BS bs
fieldlinesToFreeText3 pos (C.FieldLine pos1 bs1 : fls2@(C.FieldLine pos2 _ : _))
  -- if first line is on the same line with field name:
  -- the indentation level is either
  -- 1. the indentation of left most line in rest fields
  -- 2. the indentation of the first line
  -- whichever is leftmost
  | C.positionRow pos == C.positionRow pos1 =
      concat $
        fromUTF8BS bs1
          : mealy (mk mcol1) pos1 fls2
  -- otherwise, also indent the first line
  | otherwise =
      concat $
        replicate (C.positionCol pos1 - mcol2) ' '
          : fromUTF8BS bs1
          : mealy (mk mcol2) pos1 fls2
  where
    mcol1 = foldl' (\a b -> min a $ C.positionCol $ C.fieldLineAnn b) (min (C.positionCol pos1) (C.positionCol pos2)) fls2
    mcol2 = foldl' (\a b -> min a $ C.positionCol $ C.fieldLineAnn b) (C.positionCol pos1) fls2

    mk :: Int -> C.Position -> C.FieldLine C.Position -> (C.Position, String)
    mk col p (C.FieldLine q bs) =
      ( q
      -- in Cabal-syntax there is no upper limit, i.e. no min
      -- we squash multiple empty lines to one
      , replicate (min 2 newlines) '\n'
          ++ replicate indent ' '
          ++ fromUTF8BS bs
      )
      where
        newlines = C.positionRow q - C.positionRow p
        indent = C.positionCol q - col

mealy :: (s -> a -> (s, b)) -> s -> [a] -> [b]
mealy f = go
  where
    go _ [] = []
    go s (x : xs) = let ~(s', y) = f s x in y : go s' xs

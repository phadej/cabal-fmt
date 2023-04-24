-- |
-- License: BSD-3-Clause
-- Copyright: Oleg Grenrus
module Main (main) where

import CabalFmt.Main      (mainWithArgs)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mainWithArgs

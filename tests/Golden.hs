module Main (main) where

import Distribution.Simple.Utils  (fromUTF8BS, toUTF8BS)
import System.FilePath            ((-<.>), (</>))
import System.Process             (readProcessWithExitCode)
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString as BS

import CabalFmt         (cabalFmt)
import CabalFmt.Monad   (runCabalFmt)
import CabalFmt.Options (defaultOptions)

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ goldenTest' "cabal-fmt"
    , goldenTest' "Cabal"
    , goldenTest' "simple-example"
    ]

goldenTest' :: String -> TestTree
goldenTest' n = goldenTest n readGolden makeTest cmp writeGolden
  where
    goldenPath = "fixtures" </> n -<.> "format"
    inputPath  = "fixtures" </> n -<.> "cabal"

    readGolden  = BS.readFile goldenPath
    writeGolden = BS.writeFile goldenPath

    makeTest = do
        contents <- BS.readFile inputPath
        case runCabalFmt defaultOptions $ cabalFmt inputPath contents of
            Right output' -> return (toUTF8BS output')
            Left err      -> fail (show err)

    cmp a b | a == b    = return Nothing
            | otherwise = Just <$> readProcess' "diff" ["-u", goldenPath, "-"] (fromUTF8BS b)


    readProcess' proc args input = do
        (_, out, _) <- readProcessWithExitCode proc args input
        return out

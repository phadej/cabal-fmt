module Main (main) where

import System.FilePath            ((-<.>), (</>))
import System.IO                  (hClose, hFlush)
import System.IO.Temp             (withSystemTempFile)
import System.Process             (readProcessWithExitCode)
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map              as Map

import CabalFmt         (cabalFmt)
import CabalFmt.Monad   (runCabalFmt)
import CabalFmt.Options (defaultOptions)
import CabalFmt.Prelude

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ goldenTest' "cabal-fmt"
    , goldenTest' "Cabal"
    , goldenTest' "Cabal-notab"
    , goldenTest' "simple-example"
    , goldenTest' "tree-diff"

    , goldenTest' "fragment-missing"
    , goldenTest' "fragment-empty"
    , goldenTest' "fragment-wrong-field"
    , goldenTest' "fragment-wrong-type"
    , goldenTest' "fragment-multiple"
    , goldenTest' "fragment-section"

    , goldenTest' "issue69"
    , goldenTest' "deep-subsumption"
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
        case runCabalFmt files defaultOptions $ cabalFmt inputPath contents of
            Left err            -> fail ("First pass: " ++ show err)
            Right (output', ws) -> do
                -- idempotent
                case runCabalFmt files defaultOptions $ cabalFmt inputPath (toUTF8BS output') of
                    Left err            -> fail ("Second pass: " ++ show err)
                    Right (output'', _) -> do
                        unless (output' == output'') $ fail "Output not idempotent"
                        return (toUTF8BS $ unlines (map ("-- " ++) ws) ++ output')

    cmp a b | a == b    = return Nothing
            | otherwise =
        withSystemTempFile "cabal-fmt-test.txt" $ \fpA hdlA ->
        withSystemTempFile "cabal-fmt-test.txt" $ \fpB hdlB -> do
            BS.hPutStr hdlA a
            BS.hPutStr hdlB b
            hFlush hdlA
            hFlush hdlB
            hClose hdlA
            hClose hdlB

            Just . postProcess <$> readProcess' "diff" ["-u", fpA, fpB] ""

    postProcess :: String -> String
    postProcess = unlines . (["======"] ++) . map (concatMap char) . (++ ["======"]). lines where
        char '\r' = "{CR}"
        char c    = [c]

    readProcess' proc args input = do
        (_, out, _) <- readProcessWithExitCode proc args input
        return out

files :: Map.Map FilePath BS.ByteString
files = Map.fromList
    [ p "empty.fragment" ""

    , p "build-depends.fragment"
        "build-depends: base, doctest >=0.15 && <0.17, QuickCheck >=2.12 && <2.13, simple-example, template-haskell"

    , p "tested-with.fragment"
        "tested-with: GHC ==8.0.2"

    , p "common.fragment"
        "common deps\n  build-depends: base, bytestring, containers\n  ghc-options: -Wall"

    , p "multiple.fragment"
        "build-depends: base\nghc-options: -Wall"

    , p ("cbits" </> "header.h") "..."
    , p ("cbits" </> "source1.c") "..."
    , p ("cbits" </> "source2.c") "..."
    , p ("cbits" </> "sub" </> "source3.c") "..."
    ]
  where
    p x y = (x, BS8.pack y)

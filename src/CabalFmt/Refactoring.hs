{-# LANGUAGE OverloadedStrings #-}
module CabalFmt.Refactoring (
    Refactoring,
    refactoringExpandExposedModules,
    ) where

import Data.List       (intercalate, stripPrefix)
import Data.Maybe      (catMaybes)
import System.FilePath (dropExtension, splitDirectories)

import qualified Distribution.Compat.CharParsing     as C
import qualified Distribution.Fields                 as C
import qualified Distribution.Parsec                 as C
import qualified Distribution.Parsec.FieldLineStream as C
import qualified Distribution.Simple.Utils           as C

import CabalFmt.Comments
import CabalFmt.Options

-------------------------------------------------------------------------------
-- Refactoring type
-------------------------------------------------------------------------------

type Refactoring = Options -> [C.Field Comments] -> [C.Field Comments]

-------------------------------------------------------------------------------
-- Expand exposed-modules
-------------------------------------------------------------------------------

refactoringExpandExposedModules :: Refactoring
refactoringExpandExposedModules opts = overField refact where
    refact name@(C.Name c n) fls
        | n == "exposed-modules" || n == "other-modules"
        , definitions <- parse c =
            let newModules :: [C.FieldLine Comments]
                newModules = catMaybes
                    [ do rest <- stripPrefix prefix fp
                         return $ C.FieldLine mempty $ C.toUTF8BS $ intercalate "." rest
                    | prefix <- definitions
                    , fp <- fileList
                    ]
            in (name, newModules ++ fls)
        | otherwise = (name, fls)

    fileList :: [[FilePath]]
    fileList = map (splitDirectories . dropExtension) (optFileList opts)

    parse :: Comments -> [[FilePath]]
    parse (Comments bss) = catMaybes
        [ either (const Nothing) Just
        $ C.runParsecParser parser "<input>" $ C.fieldLineStreamFromBS bs
        | bs <- bss
        ]

    parser :: C.ParsecParser [FilePath]
    parser = do
        _ <- C.string "--"
        C.spaces
        _ <- C.string "cabal-fmt:"
        C.spaces
        _ <- C.string "expand"
        C.spaces
        dir <- C.parsecToken
        return (splitDirectories dir)

-------------------------------------------------------------------------------
-- Tools
-------------------------------------------------------------------------------

overField :: (C.Name Comments -> [C.FieldLine Comments] -> (C.Name Comments, [C.FieldLine Comments]))
          -> [C.Field Comments] -> [C.Field Comments]
overField f = goMany where
    goMany = map go

    go (C.Field name fls)       = let ~(name', fls') = f name fls in C.Field name' fls'
    go (C.Section name args fs) = C.Section name args (goMany fs)

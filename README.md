# cabal-fmt

## Usage

```sh
$ cabal install cabal-fmt
$ ...
% cabal-fmt --help
cabal-fmt - .cabal file reformatter

Usage: cabal-fmt [-i|--inplace] 
                 [--Werror | --Wno-error | --indent N | --tabular | 
                   --no-tabular] [FILE...] [--version]
  Reformat .cabal files

Available options:
  -i,--inplace             process files in-place  <-- !!! 
  --Werror                 Treat warnings as errors
  --indent N               Indentation
  --tabular                Tabular formatting
  FILE...                  input files
  -h,--help                Show this help text
  --version                Show version
```

## Output

Turns this...

```cabal
cabal-version: 2.4
name: cabal-fmt
version: 0

-- An example formatter
executable cabal-fmt
    default-language: Haskell2010
    hs-source-dirs: src 
    main-is: CabalFmt.hs
    -- build depends will be in
    -- a nice tabular format
    build-depends: base >=4.11 && <4.13, pretty >=1.1.3.6 && <1.2, bytestring, Cabal ^>=2.5, containers ^>=0.5.11.0 || ^>=0.6.0.1
    -- extensions will be sorted
    other-extensions:
      DeriveFunctor FlexibleContexts ExistentialQuantification OverloadedStrings
      RankNTypes
```

...into this:

```cabal
cabal-version: 2.4
name:          cabal-fmt
version:       0

-- An example formatter
executable cabal-fmt
  default-language: Haskell2010
  hs-source-dirs:   src
  main-is:          CabalFmt.hs

  -- build depends will be in
  -- a nice tabular format
  build-depends:
    , base        >=4.11 && <4.13
    , bytestring
    , Cabal       ^>=2.5
    , containers  ^>=0.5.11.0 || ^>=0.6.0.1
    , pretty      ^>=1.1.3.6

  -- extensions will be sorted
  other-extensions:
    DeriveFunctor
    ExistentialQuantification
    FlexibleContexts
    OverloadedStrings
    RankNTypes
```

# cabal-fmt

## Synopsis

```sh
$ cabal install cabal-fmt
$ ...
$ cabal-fmt --inplace example.cabal
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
    , base        >=4.11      && <4.13
    , bytestring
    , Cabal       ^>=2.5
    , containers  ^>=0.5.11.0 || ^>=0.6.0.1
    , pretty      >=1.1.3.6   && <1.2

  -- extensions will be sorted
  other-extensions:
    DeriveFunctor
    ExistentialQuantification
    FlexibleContexts
    OverloadedStrings
    RankNTypes
```

## Editor Integration

### Emacs

If you have `cabal-fmt` in your `$PATH` (and you use
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)),
you can auto-format `.cabal` files via
[format-all](https://github.com/lassik/emacs-format-all-the-code):

``` elisp
(use-package format-all)
(use-package haskell-cabal
  :hook
  (haskell-cabal-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))
```

### Vim

There are currently two options, both requiring `cabal-fmt` to be in your `$PATH`:

  - [`vim-cabalfmt`](https://github.com/sdiehl/vim-cabalfmt), a standalone plugin
  - [`neoformat`](https://github.com/sbdchd/neoformat), a general formatter with `cabal-fmt` support

# 0.1.13

- Use Cabal-syntax-3.14.1.0
- Drop GHC-8.4 and GHC-8.6 support

# 0.1.12

- Use Cabal-syntax-3.12.0.0

# 0.1.11

- Change how dependencies on (multiple) sublibraries are output:
  Always print each sublibrary on individual line,
  with the first one having the version range.

  ```cabal
  build-depends:
    ...
    , megalib              >=0.1 && <0.2
    , megalib:sublib-a
    , megalib:sublib-b
    , megalib:sublib-c
  ```

  instead of previous

  ```
  build-depends:
    ...
    , megalib:{megalib, sublib-a, sublib-b, sublib-c} >=0.1 && <0.2
  ```

# 0.1.10

- Fix removal of empty lines in free text fields (like `description`)
  when using `cabal-version: 3.0` (where you can use empty lines)

# 0.1.9

- Change how version ranges with carets are formatted once again.
  Now `^>=1.2 || ^>=1.3` won't be changed anymore.

# 0.1.8

- Order extensions lexicographically

# 0.1.7

- Use Cabal-syntax-3.10.1.0

# 0.1.6

- Fix filepath issues on Windows
- Add support for sublibraries
- Format `build-tool-depends`
- Use Cabal-syntax-3.8.1.0
- Smarter version range normaliser

# 0.1.5.1

- Fix bug in pretty printing empty version ranges.

# 0.1.5

- Don't print redundant `-any` in `impl`
- Add `glob-files` pragma for source file fields (`c-sources`, ...)
  Glob syntax supports only stars (i.e. no `{}` etc. extras).
- Source file fields are now sorted

# 0.1.4

- Add `-n` / `--no-cabal-file` to format cabal like, but not package files.
  Useful to format `cabal.project` or `cabal.haskell-ci`.
- Add `fragment` pragma to substitute a field or a section with
  contents of external file.
  Useful in multi-package setting to keep for example
  `tested-with` field or `common deps` stanza in sync.

# 0.1.3

- GHC-8.10 support. Require Cabal-3.2
- Add `--check` operation mode

# 0.1.2

- Don't change current working directories. Don't expand if used on stdin.
- Sort module names case-insensitively

# 0.1.1

- Change metavar of file arguments to be `FILE...`,
  to imply that multiple files at once are supported
- Add `expand src -ModuleName1 -ModuleName2` support
- Add `--tabular` and `--no-tabular` configuraiton options
- Add `-Werror` option (there are some warnings)
- Preserve end file comments
- More pretty `tested-with` formatting
- Add `--version` flag

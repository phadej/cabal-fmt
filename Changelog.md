# 0.1.3

- GHC-8.10 support. Require Cabal-3.2

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

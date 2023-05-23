One goal of these project is to be eventually merged into `cabal-install`.
However, for now the code written by me (Oleg Grenrus) is licensed under
GPL-3.0-or-later. I promise to relicense it to BSD-3-Clause for inclusion to
`cabal-install`. I already contributed the most of underlying functionality
of `cabal-fmt` to `Cabal`.

To make the future relicensing possible, I require contributions
to be made under BSD-3-Clause, making the combined license of this project
`GPL-3.0-or-later AND BSD-3-Clause`.

I'll do the necessary refactorings, so substantial new code can be
added as new modules, which should be marked with `Copyright: You` and
`License: BSD-3-Clause`.

Why such complication? Why not BSD-3-Clause to begin with?
I want this code to be first included into `cabal-install`.
One would need to do few refactoring for making configuration knobs pleasant.
If someone would like the `cabal-fmt` functionality to be included in their
proprietrary product, they could help with `cabal-install` part of the story.

---

- If you are only going to bump bounds:
  - If it's really **only bounds**, please simply open an issue (so you'll have a URL to refer to). I have a semi-automated process to make revisions, pull requests only disturb it.
  - If patch includes **source code change** (i.e. I'll need to make a release), and it's a patch to support **newer base/GHC version**:
    - Amend `tested-with` to include that GHC
    - Regenerate `.github/workflows/haskell-ci.yml` with `haskell-ci regenerate` (get the latest from [GitHub haskell-ci/haskell-ci](https://github.com/haskell-ci/haskell-ci))

- Don't edit `CHANGELOG.md`, rather include a copyable entry in your pull request description. I often process pull requests in bulk, and everyone editing the `CHANGELOG.md` causes unnecessary conflicts.
- For the same reason, do not edit `version` or `x-revision`

- I use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell) to format imports. I encourage you to use it too, when contributing.
- General code style is 4 spaces, just look around how it looks, it's not so strict.

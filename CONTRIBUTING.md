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

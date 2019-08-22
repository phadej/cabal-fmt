build :
	cabal v2-build

install :
	cabal v2-install cabal-fmt --overwrite-policy=always

self-test :
	cabal v2-run cabal-fmt -- cabal-fmt.cabal

self-test-2 :
	cabal v2-run cabal-fmt -- --no-tabular cabal-fmt.cabal

self-test-raw :
	$$(cabal-plan list-bin cabal-fmt) cabal-fmt.cabal

golden :
	cabal v2-run golden

golden-accept : 
	cabal v2-run golden -- --accept

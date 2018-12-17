self-test :
	cabal new-run --enable-tests cabal-fmt -- cabal-fmt.cabal

self-test-Cabal :
	cabal new-run --enable-tests cabal-fmt -- Cabal/Cabal.cabal

golden :
	cabal new-run --enable-tests golden

golden-accept : 
	cabal new-run --enable-tests golden -- --accept

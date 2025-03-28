build :
	cabal v2-build

install :
	cabal v2-install cabal-fmt --overwrite-policy=always

self-test :
	cabal v2-run cabal-fmt -- cabal-fmt.cabal

self-test-2 :
	cabal v2-run cabal-fmt -- --no-tabular cabal-fmt.cabal

self-test-3 :
	cabal v2-run cabal-fmt -- --no-cabal-file cabal.project

self-test-raw :
	$$(cabal-plan list-bin cabal-fmt) cabal-fmt.cabal

golden :
	cabal v2-run golden

golden-accept : 
	cabal v2-run golden -- --accept

# ALPINE BUILD

EXETARGET=cabal-fmt
VERSION=0.1.13

ALPINEVERSION:=3.17.3
GHCUPVERSION:=0.1.50.1
GHCVERSION:=9.8.4
CABALVERSION:=3.10.3.0

CABALPLAN:=$(HOME)/.local/bin/cabal-plan
CABAL:=$(HOME)/.ghcup/bin/cabal
GHC:=$(HOME)/.ghcup/bin/ghc-$(GHCVERSION)
GHCUP:=$(HOME)/.ghcup/bin/ghcup

# docker run -ti -v $(pwd):/src alpine:3.17.3
# cd /src
# apk add make
# make alpine-release
#
.PHONY: alpine-release
alpine-release :
	apk add binutils-gold curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev openssh-client perl tar tmux vim xz xz-dev zlib-dev zlib-static
	mkdir -p $(HOME)/.local/bin
	curl -L https://github.com/haskell-hvr/cabal-plan/releases/download/v0.7.3.0/cabal-plan-0.7.3.0-x86_64-linux.xz | xz -d > $(CABALPLAN)
	chmod a+x $(CABALPLAN)
	mkdir -p $(HOME)/.ghcup/bin
	curl https://downloads.haskell.org/~ghcup/$(GHCUPVERSION)/x86_64-linux-ghcup-$(GHCUPVERSION) > $(GHCUP)
	chmod a+x $(GHCUP)
	$(GHCUP) install ghc $(GHCVERSION)
	$(GHCUP) install cabal $(CABALVERSION)
	$(CABAL) update --ignore-project
	$(CABAL) build exe:$(EXETARGET) -fexe --with-compiler $(GHC) --enable-executable-static
	strip $$($(CABALPLAN) list-bin $(EXETARGET))
	@ls -l $$($(CABALPLAN) list-bin $(EXETARGET))
	cat $$($(CABALPLAN) list-bin $(EXETARGET)) | xz > $(EXETARGET)-$(VERSION)-x86_64-linux.xz
	@ls -l $(EXETARGET)-$(VERSION)-x86_64-linux.xz
	sha256sum $(EXETARGET)-$(VERSION)-x86_64-linux.xz | tee $(EXETARGET)-$(VERSION).SHA256SUM

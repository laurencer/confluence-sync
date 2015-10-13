all: build

#
# Top-level targets. This is ugly. A program to extract these from the .cabal
# file would work, but is there anything easier?
#

snippet: dist/build/snippet/snippet
check: dist/build/check/check

#
# Setup
#

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=2>/dev/null
endif

.PHONY: all build test

#
# Build rules. This just wraps Cabal doing its thing in a Haskell
# language-specific fashion.
#

build: dist/setup-config tags
	@/bin/echo -e "CABAL\tbuild"
	cabal build http-streams

test: dist/setup-config tags
	@/bin/echo -e "CABAL\ttest"
	cabal build check
	dist/build/check/check

dist/setup-config: http-streams.cabal .cabal-sandbox/add-source-timestamps Setup.hs
	cabal configure \
		--enable-tests \
		--enable-benchmarks \
		-v0 2>/dev/null || /bin/echo -e "CABAL\tinstall --only-dependencies" && cabal install --only-dependencies -j --enable-tests --enable-benchmarks
	@/bin/echo -e "CABAL\tconfigure"
	cabal configure \
		--enable-tests \
		--enable-benchmarks \
		--disable-library-profiling


# This will match writer-test/writer-test, so we have to strip the directory
# portion off. Annoying, but you can't use two '%' in a pattern rule.
dist/build/%: dist/setup-config tags $(SOURCES)
	@/bin/echo -e "CABAL\tbuild $@"
	cabal build $(notdir $@)

#
# Build ctags file
#

SOURCES=$(shell find lib -name '*.hs' -type f) \
	$(shell find tests -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

tags: $(SOURCES)
	if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	-$(CTAGS) $^ > tags $(REDIRECT)

format: $(SOURCES)
	stylish-haskell -i $^

clean:
	@/bin/echo -e "CABAL\tclean"
	-cabal clean >/dev/null
	@/bin/echo -e "RM\ttemporary files"
	-rm -f tags
	-rm -f *.prof
	-rm -f lib/Package.hs

doc:
	cabal haddock

install:
	cabal install

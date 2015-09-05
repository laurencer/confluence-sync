GHC = ghc
GHCFLAGS = -O2 -package haxr -XOverlappingInstances

TEST_PROGS = make-stubs parse_response \
             person_server person_client raw_call \
	     simple_client simple_server test_client \
             test_server time-xmlrpc-com validate \
             person_server person_client

.SUFFIXES: .hs .hi .o

.PHONY: all clean

default all: $(TEST_PROGS)

%: %.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

clean:
	-rm -f *.hi *.o $(TEST_PROGS)

clean:
	rm arithmetic
all:
	cabal install bitstring
	ghc -o arithmetic haskar.hs

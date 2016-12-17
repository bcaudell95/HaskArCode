clean:
	rm arithmetic
all:
	cabal update
	cabal install bitstring
	ghc -o arithmetic haskar.hs

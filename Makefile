all:
	cabal update
	cabal install bitstring
	ghc -o arithmetic haskar.hs
clean:
	rm arithmetic

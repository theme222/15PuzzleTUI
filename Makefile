linux:
	@echo "Building for linux..."
	cabal build
	cp `cabal list-bin 15p` .
install:
	cabal install
clean:
	@echo "Cleaning..."
	cabal clean
	rm -f 15p
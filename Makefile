.PHONY: build_project
build_project:
	cabal update && cabal install QuickCheck-2.4 && cabal configure && cabal build

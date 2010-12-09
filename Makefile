.PHONY: build_project
build_project:
	cabal update && cabal install --prefix=`pwd`/install

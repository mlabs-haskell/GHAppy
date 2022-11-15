#!/usr/bin/env sh
cabal run GHAppy -- \
	-a $(cat ./token.txt) \
	-o out -f outFile -r "mlabs-haskell/optim-onchain-audit" \
	-u "cstml"

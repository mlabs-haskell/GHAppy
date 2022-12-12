#!/usr/bin/env sh
# or nix run github:mlabs-haskell/GHAppy

cabal run GHAppy -- \
	-a $(cat ./token.txt) \
	-o "out" \
	-f "outFile" \
	-r "mlabs-haskell/optim-onchain-audit" \
	-u "cstml" \
	-i ./example/report2.yaml

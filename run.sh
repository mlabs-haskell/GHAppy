#!/usr/bin/env sh
# or nix run github:mlabs-haskell/GHAppy

cabal run GHAppy -- \
	-a $(cat ./.token.txt) \
	-o "_out" \
	-f "outFile" \
	-r "mlabs-haskell/indigo-smart-contracts-audit-2" \
	-u "IAmPara0x" \
	-i ./example/report2.yaml

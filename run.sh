#!/usr/bin/env sh
# or nix run github:mlabs-haskell/GHAppy

cabal run GHAppy -- \
	-a $(cat ./token.txt) \
	-o "out" \
	-f "outFile" \
	-r "IndigoProtocol/smart-contracts-aiken" \
	-u "IAmPara0x" \
	-i ./example/report2.yaml

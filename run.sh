#!/usr/bin/env sh
# or nix run github:mlabs-haskell/GHAppy

cabal run GHAppy -- \
	-a $(cat ./.token.txt) \
	-o "_out" \
	-f "outFile" \
	-r "jpg-store/vesting-contract" \
	-u "IAmPara0x" \
	-i ./example/report2.yaml

#!/usr/bin/env sh
curl \
	-H "Accept: application/vnd.github+json" \
	-H "Authorization: Bearer $(cat ./token.txt)" \
	https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues

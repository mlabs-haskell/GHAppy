#!/usr/bin/env sh
curl -G -d "per_page:1" \
	-H "Accept: application/vnd.github+json" \
	-H "Authorization: Bearer $(cat ./token.txt)" \
	https://api.github.com/repos/mlabs-haskell/optim-onchain-audit/issues?per_page=100 &
direction="asc"

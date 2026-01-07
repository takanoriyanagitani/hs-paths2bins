#!/bin/sh

find . -type f -name '*.hs' |
	xargs \
		fourmolu \
		--no-cabal \
		--mode inplace

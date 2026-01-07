#!/bin/sh

find \
	. \
	-type f |
	./hs-paths2bins

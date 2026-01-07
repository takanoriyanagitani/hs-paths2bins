#!/bin/bash

dname=dist-newstyle/build/aarch64-osx/ghc-9.4.8/hs-paths2bins-0.1.0.0/doc/html/hs-paths2bins
port=10581
addr=127.0.0.1

miniserve \
    --port ${port} \
    --interfaces "${addr}" \
    "${dname}"

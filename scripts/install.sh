#!/usr/bin/env bash -eu

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal update
cabal install happy alex
cabal install\
  --force-reinstalls --reorder-goals --only-dependencies --enable-tests\
  "${path[@]}"

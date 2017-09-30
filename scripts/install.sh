#!/usr/bin/env bash -eu


FILE="nr_agent_sdk-v0.16.2.0-beta.x86_64.tar.gz"
rm -f FILE
wget "http://download.newrelic.com/agent_sdk/nr_agent_sdk-v0.16.2.0-beta.x86_64.tar.gz"
tar xvf $FILE
NEWRELIC_BASE=`pwd`/${FILE%.tar.gz}
export LD_LIBRARY_PATH=$NEWRELIC_BASE/lib

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal update
cabal install\
  --force-reinstalls --reorder-goals --only-dependencies --enable-tests\
  --extra-include-dirs=$NEWRELIC_BASE/include\
  --extra-lib-dirs=$NEWRELIC_BASE/lib\
  "${path[@]}"

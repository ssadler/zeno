#!/bin/bash


function dev () {
  ghcid -c 'stack repl zeno:lib'
}

function devtest () {
  ghcid -c 'stack repl zeno --test --main-is :zeno-test' --test ':main --color always'
}

PATH="../komodo/src/:$PATH"

function runKmdDev () {
  komodod -ac_name=TXSCLZDEV -ac_supply=999999 -ac_reward=1000000 \
      -ac_blocktime=3 \
      -addressindex=1 \
      -ac_nk=96,5 \
      -ac_sapling=2 \
      -testnode=1 &
  cpulimit -l 33 -p $!
}

function queryKmdDev () {
  komodo-cli --ac_name=TXSCLZDEV $@
}

function dot () {
    # eg: ./manage.sh dot | xdot -
    graphmod -
    # --collapse=Module
}

function install-fast() {
  build-fast
  stack install
}

fast_build_ghc_ops="-j4 +RTS -A128m -n2m -qg -RTS"

function build-fast() {
  stack build --fast --ghc-options $fast_build_ghc_ops
}

function test-fast() {
  stack test --ghc-options $fast_build_ghc_ops
}

function build-profile () {
  stack build --profile --ghc-options=-fprof-auto-top
}

cmd=$1;
if [[ "$cmd" == "" ]]; then
  echo "Usage: ./manage.sh COMMAND [ARGS]";
  exit 1;
fi
shift
$cmd $@

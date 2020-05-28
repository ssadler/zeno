#!/bin/bash

komodod -ac_name=TXSCLZDEV -ac_supply=999999 \
    -ac_blocktime=10 -ac_nk=96,5 \
    -testnode=1 &

cpulimit -l 33 -p $!

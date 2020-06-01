#!/bin/bash

set -e


# for l in `cat integration/integrate.sh  | grep '^wif' | pyline 'l[6:-1]'`; do ../queryzeno.sh importprivkey $l; done
# for l in `cat integration/integrate.sh  | grep '^addr' | pyline 'l[7:-1]'`; do ../queryzeno.sh sendtoaddress $l 1; done

sk0="4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d"
wif0="UrfazJ3uUn8pDuyrohe6dsGHHUw76yuNkSkHdBG8s9aXdtH4jyCR"
pk0="03e68acfc0253a10620dff706b0a1b1f1f5833ea3beb3bde2250d5f271f3563606"
addr0="RWgagrqdN7YWH4N6kB4mWCNPCgtAMkCLFp"

sk1="6cbed15c793ce57650b9877cf6fa156fbef513c4e6134f022a85b1ffdd59b2a1"
wif1="UsevvaX4V7Uz6L8Jnj2PT6Q7btMFabtS1RECAXiAzxy5jUxVYtFX"
pk1="0294d3137ac5561a2f7b7909f3304c678bbe5c628c4156e1f9ef40c521b41b9d27"
addr1="RC2cZ25L6ueuy66WB6MjFEgP14JU2gNxhu"

sk2="6370fd033278c143179d81c5526140625662b8daa446c22ee2d73db3707e620c"
wif2="UsLqwwYWsWokLHSvjMbsukT7svni1D64j6Tsmc7ugziUSKDDT7Aw"
pk2="0356e12fd39278e33cd2df2aaa0acd57974a1ce5637e2231903dc0bf45aeffb9a3"
addr2="RHPT5v4o6o93eofnPGmD53LK3hTo1WaZEx"

sk3="646f1ce2fdad0e6deeeb5c7e8e5543bdde65e86029e2fd9fc169899c440a7913"
wif3="UsNmsDB7ce2XZEfu2foQ3u5bLm66WWyoZnaF41rP9gqSWTdEaAE6"
pk3="0278b7e11a2b1b66504963ba9cfcccc69a37b6fc8138e51b6e82effba9dc6f8d81"
addr3="RL72qw2ymk56DzXk7mW36VvhiKwWzjggap"


stack build --fast

# 2 windows
tmux split-window -h \; split-window -h \; select-layout even-horizontal
tmux split-window -v -t 0
tmux split-window -v -t 2
tmux split-window -v -t 4

CHAIN=TXSCLZDEV

notarise="stack exec zeno -- notarise"
ethkmd="kmdeth --ui --gateway=$1 --seed=127.0.0.1:40440 --kmd=~/.komodo/$CHAIN/$CHAIN.conf --geth=http://127.0.0.1:9545/"

tmux send-keys -t 1 "$notarise seed --bind=127.0.0.1 --port=40440" Enter
tmux send-keys -t 2 "$notarise $ethkmd --pubkey=$pk0 --port=40441" Enter
tmux send-keys -t 3 "$notarise $ethkmd --pubkey=$pk1 --port=40442" Enter
tmux send-keys -t 4 "$notarise $ethkmd --pubkey=$pk2 --port=40443" Enter
tmux send-keys -t 5 "$notarise $ethkmd --pubkey=$pk3 --port=40444" Enter

tmux select-pane -t 0


function ctrl_c() {
  for i in $(seq 5 -1 1); do
    tmux send-keys -t $i C-c C-c Enter "exit" Enter
  done
};

trap ctrl_c INT

sleep 100000000

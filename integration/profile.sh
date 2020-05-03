#!/bin/bash

set -e


sk0="4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d"
#(PrvKey "UrfazJ3uUn8pDuyrohe6dsGHHUw76yuNkSkHdBG8s9aXdtH4jyCR",PubKey "03e68acfc0253a10620dff706b0a1b1f1f5833ea3beb3bde2250d5f271f3563606",Address "RWgagrqdN7YWH4N6kB4mWCNPCgtAMkCLFp")
sk1="6cbed15c793ce57650b9877cf6fa156fbef513c4e6134f022a85b1ffdd59b2a1"
#(PrvKey "UsevvaX4V7Uz6L8Jnj2PT6Q7btMFabtS1RECAXiAzxy5jUxVYtFX",PubKey "0294d3137ac5561a2f7b7909f3304c678bbe5c628c4156e1f9ef40c521b41b9d27",Address "RC2cZ25L6ueuy66WB6MjFEgP14JU2gNxhu")
sk2="6370fd033278c143179d81c5526140625662b8daa446c22ee2d73db3707e620c"
#(PrvKey "UsLqwwYWsWokLHSvjMbsukT7svni1D64j6Tsmc7ugziUSKDDT7Aw",PubKey "0356e12fd39278e33cd2df2aaa0acd57974a1ce5637e2231903dc0bf45aeffb9a3",Address "RHPT5v4o6o93eofnPGmD53LK3hTo1WaZEx")
sk3="646f1ce2fdad0e6deeeb5c7e8e5543bdde65e86029e2fd9fc169899c440a7913"
#(PrvKey "UsNmsDB7ce2XZEfu2foQ3u5bLm66WWyoZnaF41rP9gqSWTdEaAE6",PubKey "0278b7e11a2b1b66504963ba9cfcccc69a37b6fc8138e51b6e82effba9dc6f8d81",Address "RL72qw2ymk56DzXk7mW36VvhiKwWzjggap")



stack build --profile --ghc-options=-fprof-auto-top
RTS="+RTS -p"

tmux split-window -h \; split-window -h \; select-layout even-horizontal

tmux split-window -v -t 0
tmux split-window -v -t 1
tmux split-window -v -t 2

notarise="stack exec hath -- notarise"
ethkmd="ethkmd --mandate=0x8d555026e8952720ebeeda3269833e3626e484eb --seed=127.0.0.1:40440 --host=127.0.0.1"

tmux send-keys -t 3 "$notarise seed --host=127.0.0.1 --port=40440" Enter
tmux send-keys -t 1 "$notarise $ethkmd --address=RWgagrqdN7YWH4N6kB4mWCNPCgtAMkCLFp --port=40441 $@ $RTS" Enter
tmux send-keys -t 2 "$notarise $ethkmd --address=RC2cZ25L6ueuy66WB6MjFEgP14JU2gNxhu --port=40442 $@" Enter
tmux send-keys -t 4 "$notarise $ethkmd --address=RHPT5v4o6o93eofnPGmD53LK3hTo1WaZEx --port=40443 $@" Enter
tmux send-keys -t 5 "$notarise $ethkmd --address=RL72qw2ymk56DzXk7mW36VvhiKwWzjggap --port=40444 $@" Enter

tmux select-pane -t 0

function ctrl_c() {
    for i in $(seq 5 -1 1); do
        tmux send-keys -t $i C-c C-c Enter "exit" Enter
    done
};

trap ctrl_c INT

sleep 100000000

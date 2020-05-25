# zeno

Notarising software for Komodo <> Ethereum, and eventually other protocols.

## Building

On Ubuntu 18.04:

```
sudo apt-get install libsecp256k1-dev pkg-config
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/ssadler/zeno
cd zeno
stack build
```

## Notes

Each consensus round is a series of steps. In Zeno, each step has a topic, which is a hash of the data that they are voting on. Nodes listen for messages with that topic. For each topic, each member can cast 1 vote. The step process is to synchronise all the votes, which are collected into an Inventory. In the case where there is a proposer, we stop bothering to build inventory when we have the vote from the chosen proposer.

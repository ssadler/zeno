# zeno

Fancy new notarising software for Komodo <> Ethereum

## Building

On Ubuntu 18.04:

```
sudo apt-get install libsecp256k1-dev pkg-config
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/ssadler/zeno
cd zeno
stack build
```

## Dev notes

* Ok, I spent AGES fighting with the signatures. Learnings:

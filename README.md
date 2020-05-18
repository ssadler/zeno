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

# Zeno

Notarising software for Komodo <> Ethereum, and eventually other protocols.

## Building

On Ubuntu 18.04:

```
sudo apt-get install libsecp256k1-dev pkg-config
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/ssadler/zeno
cd zeno
stack install
```

## Why "Zeno"?

The name is from the Greek philosopher, Zeno of Elea.

## What are the goals of this project?

To enable a coordinated group of signatories to act as an oracle and deliver messages between blockchains.

It includes a simple consensus mechanism (with caveats), and code to interact with several different blockchains, currently including Komodo, Ethereum, and theoreteically Bitcoin and ZCash.

## How does consensus work in Zeno?

The consensus process is a round comprised of many steps, where each step represents voting on a particular topic, or selecting a proposer when there's no obvious way to get a determinable result.

However, the limitation of the consensus process is that currently it does not provide any kind of finality, or solve the two generals problem. Zeno is itself stateless and relies on the chains it is posting messages to to provide finality instead. So at a given point in time, it can query the blockchain for the last recorded action and resume from that point.

## Message protocol

For now this is not a complete description of the message protocol.

### Message protocol

For now this is not a complete description of the protocol.

A round message in Zeno looks like:

`{8 bytes length}{16 bytes process ID}{66 bytes signature}{step num}{object}`

The step num identifies the type of data in a step, and consists of the following:

```
1 1 3 0
| | | ^ Minor step number, usually 0, usually a retry of a previous step e.g. in case of proposer timeout
| | ^   Major step number, indicates the data type being exchanged in the step.
| ^     Round ID, 
^       Prefix, indicating whether or not the 3 bytes following will be present.

0
^ The step number prefix is 0, indicating that 
```

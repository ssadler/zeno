# Changelog for Zeno

# 0.2.1.2

* Fix bugs in step numbers

## 0.2.1.1

* Protocol changes to make it easier to parse with PyZeno

## 0.2.1.0

* All incoming messages as well as incoming inventory is now signed and authenticated
* Proposer timeouts are recorded back to Komodo and there's a CLI command to retrieve them
* Doc for install and elasticsearch log by TonyL
* Round members are now part of round seed
* UTXOs from Komodo have an internal lock so they aren't allocated twice
* Add type identifiers to the step datagrams, so that light clients can know what they are looking at
* Combined inventory broadcast message (request, index, data) so the algorithm can be tweaked without protocol change
* Ignore SIGPIPE signal which is sometimes dispatched by the OS
* Close sockets when done with inbound connections
* Remove a thread from broadcast
* KMD UTXO size is 9850 to not conflict with other programs consuming UTXOs
* Bug in secp256k1-haskell library mitigated

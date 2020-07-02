# Changelog for Zeno

## 0.3.0.0-rc1 - 02/7/2020

* Remove proposer timeouts, use UTXO selection + some magic to derive statistics
* Overhaul gossip layer, make consensus process testable
* Address memory leak described in #20
* Get IP from icanhazip.com
* Do something more sensible for inbound connection limit
* fix #21; Must filter out p2pkh UTXOs from listunspent
* fix deadlock due to receiveCache writing to bounded queue
* Fix UI so it prints useful info
* Many other bugfixes and improvements

## 0.2.3.1 - 19/6/2020

* Upgrade version of secp256k1 wrapper

## 0.2.3.0 - 17/6/2020

* Make proposer round robin activation configurable

## 0.2.2.2 - 16/6/2020

* More debug messages for sync notariser
* Remove dependency on `sys/random.h` which isn't present on some systems

## 0.2.2.1 - 16/6/2020

* add `./manage.sh test-fast`
* Fix some tests that got broken.

## 0.2.2.0 - 15/6/2020

* Print version info on startup
* Reload config more often during normal operation
* Separate core synchronous notarising logic into separate module, write tests for module
* Use block interval for both ends, kmd and eth also, and record eth block height, to prevent getting stuck in a round when no proposers are available
* Add abstract interface for blockchain endpoints so it's easier to add new ones
* Swap out bindings to Secp256k1 for less buggy version that supports recovery
* More detail in dumpProposerTimeouts error messages
* Bugfix for dumpProposerTimeouts

## 0.2.1.2

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

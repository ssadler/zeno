## 1. Zeno startup

#### 1.1 Build zeno according to actual instructions:

https://github.com/ssadler/zeno#building

#### 1.2 Install, start and sync geth node for Ropsten testnet:

https://geth.ethereum.org/docs/install-and-build/installing-geth

```
sudo add-apt-repository -y ppa:ethereum/ethereum
sudo apt-get update
sudo apt-get install ethereum
```

To start in light mode: 

`geth --testnet --syncmode light --rpc`

Usually there is not enough light mode ropsten peers so most probably you'll need to start geth in full mode:

`geth --testnet --rpc console`

When you get zeno installed and geth node synced, run:

`zeno util fromPub <yournotarypk>`

and get your eth address, then get some ropsten ether from https://faucet.ropsten.be/

#### 1.3 Run and sync TXSCLZ chain:

Use https://github.com/KomodoPlatform/komodo master branch

Chain params:

```
./komodod -ac_name=TXSCLZ -ac_supply=999999 -ac_reward=100000000 -ac_sapling=375 -addnode=195.201.137.5 
```

**Do not forget to import privkey for your notary pubkey**

#### 1.4 Start zeno with output redirecting to file, e.g. :

change:

```json
--pubkey param to your notary pubkey
--bind param to your node white IP
```

```
zeno notarise kmdeth --geth http://127.0.0.1:8545 --port 7766 --gateway 0x0E27bd633C31AaE55079ee41e01579e2333e3cb2 --pubkey 02b61089cfab5f3db62d78d8de8189dc591d7a4688d0b69204df9572dfeed780c9 --kmd ~/.komodo/TXSCLZ/TXSCLZ.conf --seed 195.201.20.230:40440 --bind 95.216.204.220 >> zeno.log
```

## 2. Filebeat logging system install and configuring

#### 2.1 Install filebeat

```
curl -L -O https://artifacts.elastic.co/downloads/beats/filebeat/filebeat-7.7.1-amd64.deb
sudo dpkg -i filebeat-7.7.1-amd64.deb
```

#### 2.2  Modify /etc/filebeat/filebeat.yml filebeat settings

##### 2.2.1 enable logging and setup path to it in filebeat.inputs

```
enabled: true
paths:
- /home/tony/zeno/zeno.log
```

(path to your log file might be different)

##### 2.2.2 Specify cloud.id and cloud.auth for auth - cloud.id is the same for everyone, for cloud.auth use special username/password you've received in DM

```
cloud.id: "Test_Kibana:dXMtZWFzdC0xLmF3cy5mb3VuZC5pbyRmMGQwN2E4MjU0ZjU0MjY4YjgwYjcwNzYxZGJjYWMzYyQ2YzA0ZmNjM2QzYjY0N2NlOWI3ZTY1ZjBiYjlmNzdlMg=="
cloud.auth: "username:password"
```

##### 2.2.3 Add your notary nickname as metadata

```
processors:
  - add_host_metadata:
      name: tonyl
  - add_cloud_metadata: ~
  - add_docker_metadata: ~
  - add_kubernetes_metadata: ~
```

Example of such config: https://gist.github.com/tonymorony/23f95d0f56f594b1c58501342bb17e73


##### 2.2.4 Start filebeat

`sudo service filebeat start`


##### 2.2.5 Test that your configuration is fine

```bash
filebeat test output

elasticsearch: https://f0d07a8254f54268b80b70761dbcac3c.us-east-1.aws.found.io:443...
  parse url... OK
  connection...
    parse host... OK
    dns lookup... OK
    addresses: 52.45.248.167, 35.175.163.91, 52.22.162.248, 54.88.54.135, 52.0.247.45, 54.173.105.155, 34.226.182.57, 52.20.202.126
    dial up... OK
  TLS...
    security: server's certificate chain verification is enabled
    handshake... OK
    TLS version: TLSv1.2
    dial up... OK
  talk to server... OK
  version: 7.7.1
```

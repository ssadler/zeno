#!/usr/bin/env python3
import binascii
import gevent
import struct
from gevent import socket


def init_peer(peer, port):
    sends = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sends.connect(peer)
    sends.sendall(b'\0' + struct.pack('>H', port))
    while True:
        sends.sendall(  binascii.unhexlify(b'00000011e4dfbbb9aeaff2c844181d5f031f2cac00'))

PEER = ('127.0.0.1', 4040)

if __name__ == '__main__':
    what = []
    what.append(gevent.spawn(init_peer, PEER, 8888))
    gevent.joinall(what)

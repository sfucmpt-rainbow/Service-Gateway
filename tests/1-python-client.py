#!/usr/bin/env python2
''' 
Sends a ping to the Erlang server and
verifies echo back.
'''
import sys
import socket

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('localhost', 7000))
s.send("ping")
echo = s.recv(32)
s.close()

if echo == 'ping':
	sys.exit(0)
else:
	sys.exit(1)

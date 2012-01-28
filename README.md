Rainbow Service Gateway (rsg)
=============================

Purpose
-------

The service gateway makes use of Erlang's powerful interface for writing correct concurrent code. The main purpose of this service is for job dispatch to backend workers (controller nodes). It is responsible for keeping track of available nodes until hand-off to the cluster controllers. The node and rsg should communicate using Erlang's interface for message passing.

Installing
----------

1. Clone source code
2. cd into source code directory
3. make && make install
4. make clean (optional, removes binaries)
5. Test by going into the erl shell and issuing:
	1> rsg:start().
	2> i().

Testing
-------
Go on a host which has connection to the rsg host. Make sure that port 7000 is available:
	$ telnet $rsg_host 7000 
	$ ping
You should receive a ping back.

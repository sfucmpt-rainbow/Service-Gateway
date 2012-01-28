-module(sg).
-export([start/0, loop0/1, worker/2]).

-define(PORTNO, 7000).
-define(POOL, 10).

start() ->
	start(?PORTNO).
start(Port) ->
	spawn(?MODULE, loop0, [Port]).

loop0(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
		{ok, LSock} ->
			spawnPool(?POOL, LSock, self()),
			loop(LSock);
		_ ->
			stop
	end.

spawnPool(0, _, _) -> ok;
spawnPool(N, LSock, Server) ->
	spawn(?MODULE, worker, [Server, LSock]),
	spawnPool(N - 1, LSock, Server).
	

loop(Listen) ->
	receive
		next_worker ->
			spawn_link(?MODULE, worker, [self(), Listen])
	end,
	loop(Listen).
	

worker(Server, Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			Server ! next_worker,
			echo(Socket);
		{error, Reason} ->
			Server ! next_worker,
			io:format("Can't accept socket ~p~n", [Reason])
	end.

echo(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Socket, Packet),
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Packet rejected because: ~p~n", [Reason])
	end.


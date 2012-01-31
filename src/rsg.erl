-module(rsg).
-export([start/0, stop/1, loop0/1, worker/2, queryMaster/3, queryWorkerController/1, queryWorkerProcess/2]).

-define(PORTNO, 7000).
-define(PORTAPI, 7001).
-define(MAXQUERY, 100).
-define(POOL, 10).

start() ->
	start(?PORTNO).
start(Port) ->
	spawn(?MODULE, loop0, [Port]),
	register(queryMaster, spawn(?MODULE, queryMaster, [?PORTAPI, queue:new(), gen_query_ids(?MAXQUERY)])).

stop([Node]) ->
	case rpc:call(Node, init, stop, []) of
		{badrpc, Reason} ->
			io:format("Failed to halt '~p' because ~p~n", [Node, Reason]);
		_ ->
			ok
	end, 
	init:stop().

gen_query_ids(0) ->
	[];
gen_query_ids(N) ->
	[ N | gen_query_ids(N - 1)].

queryMaster(Port, Q, Ids) ->
	case gen_tcp:listen(Port, [list, {packet, 0}, {active, false}]) of
		{ok, LSock} ->
			Controller = spawn_link(?MODULE, queryWorkerController, [LSock]),
			Controller ! next_worker;
		_ ->
			io:format("Failed to start query manager."),
			stop
	end,
	queryMasterLoop(Q, Ids).

queryMasterLoop(Q, Ids) ->
	receive
		{getQueue, Pid} ->
			Pid ! queue:to_list(Q),
			queryMasterLoop(Q, Ids);
		{putHash, Hash, Pid} ->
			{Q2, Ids2, Result, Id} = enqueue(Q, Hash, Ids),
			Pid ! {Result, Id},
			queryMasterLoop(Q2, Ids2)
	end.

enqueue(Q, Hash, Ids) ->
	if 
		length(Ids) > 0 ->
			Id = lists:nth(1, Ids),
			Ids2 = lists:delete(Id, Ids),
			Q2 = queue:in(Hash, Q),
			{Q2, Ids2, ok, Id}; 
		true ->
			{Q, Ids, full, -1}
	end.

queryWorkerController(LSock) ->
	receive 
		next_worker ->
			spawn_link(?MODULE, queryWorkerProcess, [self(), LSock])
	end,
	queryWorkerController(LSock).
			
queryWorkerProcess(Controller, LSock) ->
	case gen_tcp:accept(LSock) of
		{ok, Socket} -> 
			Controller ! next_worker,
			handleProtocol(Socket);
		{error, Reason} ->
			Controller ! next_worker
	end.

sanitizePacket(Packet) ->
	lists:nth(1, string:tokens(Packet, "\r\n ")).

handleProtocol(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			case list_to_binary(sanitizePacket(Packet)) of
				<<"GET">> ->
					handleGetQueue(Socket);
				<<"QUERY">> ->
					handlePutQuery(Socket);
				<<"REGISTER">> ->
					ok;
				_ ->
					gen_tcp:send(Socket, <<"Unkown method">>)
			end,
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Packet rejected because: ~p~n", [Reason])
	end.

handleGetQueue(Socket) ->
	queryMaster ! {getQueue, self()},
	receive
		QueueList ->
			gen_tcp:send(Socket, list_to_binary(string:join(QueueList, " ")))
	end.

handlePutQuery(Socket) ->
	gen_tcp:send(Socket, <<"Ship it\n">>),
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			Hash = sanitizePacket(Packet),
			queryMaster ! {putHash, Hash, self()},
			receive 
				{ok, Id} ->
					gen_tcp:send(Socket, list_to_binary(io_lib:format("~p", [Id])));
				{full, _} ->
					gen_tcp:send(Socket, <<"full">>);
				_ ->
					gen_tcp:send(Socket, <<"failed">>)
			end;
		{error, Reason} ->
			ok
	end.

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


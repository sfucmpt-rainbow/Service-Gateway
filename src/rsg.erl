% Rainbow Service Gateway.
% Handles SIMPLE calls and manages the global request queue
% highly multithreaded for scalability and speed.
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

% Generates a list of available job ids, once we've run out, we cannot add anymore to the queue
% Jobs can finish at different times, so we want a number of unique ids we can distribute
gen_query_ids(0) ->
	[];
gen_query_ids(N) ->
	[ N | gen_query_ids(N - 1)].

% Queue master bootstrap function
queryMaster(Port, Q, Ids) ->
	% Creates a TCP socket for the workers
	case gen_tcp:listen(Port, [list, {packet, 0}, {active, false}]) of
		{ok, LSock} ->
			Controller = spawn_link(?MODULE, queryWorkerController, [LSock]),
			% Spawn one worker to start
			Controller ! next_worker;
		_ ->
			% Could not listen, give up
			io:format("Failed to start query manager."),
			stop
	end,
	queryMasterLoop(Q, Ids).

% Queue master loop, in charge of the globally shared job queue
queryMasterLoop(Q, Ids) ->
	% wait for messages from other processes
	receive
		% Returns a list representation of the current queue
		{getQueue, Pid} ->
			Pid ! queue:to_list(Q),
			queryMasterLoop(Q, Ids);
		% Adds a Hash query to the job queue
		{putHash, Hash, Pid} ->
			% SSA, blargh! get the results as well as the new queue and id list
			{Q2, Ids2, Result, Id} = enqueue(Q, Hash, Ids),
			Pid ! {Result, Id},
			queryMasterLoop(Q2, Ids2)
	end.

% Adds a hash to the queue and decrement Ids or 
enqueue(Q, Hash, Ids) ->
	if 
		% we still have ids to give out
		length(Ids) > 0 ->
			Id = lists:nth(1, Ids),             % pops the first
			Ids2 = lists:delete(Id, Ids),
			Q2 = queue:in(Hash, Q),             % insert Hash
			{Q2, Ids2, ok, Id};                 % return updated
		% no more, failed, use status quo for queues
		true ->
			{Q, Ids, full, -1}
	end.

% Worker spawner
queryWorkerController(LSock) ->
	receive 
		next_worker ->
			spawn_link(?MODULE, queryWorkerProcess, [self(), LSock])
	end,
	queryWorkerController(LSock).
			
% Actual worker
queryWorkerProcess(Controller, LSock) ->
	% take the socket to server
	case gen_tcp:accept(LSock) of
		{ok, Socket} -> 
			% spawns next
			Controller ! next_worker,
			handleProtocol(Socket);
		{error, Reason} ->
			Controller ! next_worker
	end.

% Gets rid of the trailing whitespace near the end of the input
sanitizePacket(Packet) ->
	lists:nth(1, string:tokens(Packet, "\r\n ")).

% Micro protocol akin to HTTP. Stateful transactions.
handleProtocol(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			% Method matching (BINARY!)
			case list_to_binary(sanitizePacket(Packet)) of
				% gets the job queue
				<<"GET">> ->
					handleGetQueue(Socket);
				% puts a query (two transactions)
				<<"QUERY">> ->
					handlePutQuery(Socket);
				% Registers a node
				<<"REGISTER">> ->
					ok;
				% not implemented
				_ ->
					gen_tcp:send(Socket, <<"Unkown method">>)
			end,
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Packet rejected because: ~p~n", [Reason])
	end.

% sends the queue (encoded in ascii) back to the client
handleGetQueue(Socket) ->
	queryMaster ! {getQueue, self()},
	receive
		QueueList ->
			gen_tcp:send(Socket, list_to_binary(string:join(QueueList, " ")))
	end.

% asks the queue master to insert a new hash
handlePutQuery(Socket) ->
	gen_tcp:send(Socket, <<"Ship it\n">>),
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			% cleanup the string
			Hash = sanitizePacket(Packet),
			% send request
			queryMaster ! {putHash, Hash, self()},
			receive 
				% okay, job inserted
				{ok, Id} ->
					% send back the job id
					gen_tcp:send(Socket, list_to_binary(io_lib:format("~p", [Id])));
				% failed, queue is full
				{full, _} ->
					gen_tcp:send(Socket, <<"full">>);
				% failed, don't know why
				_ ->
					gen_tcp:send(Socket, <<"failed">>)
			end;
		{error, Reason} ->
			ok
	end.

% echo loop
loop0(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
		{ok, LSock} ->
			spawnPool(?POOL, LSock, self()),
			loop(LSock);
		_ ->
			stop
	end.

% spawns a pool of standby processes
spawnPool(0, _, _) -> ok;
spawnPool(N, LSock, Server) ->
	spawn(?MODULE, worker, [Server, LSock]),
	spawnPool(N - 1, LSock, Server).
	

% worker spawner
loop(Listen) ->
	receive
		next_worker ->
			spawn_link(?MODULE, worker, [self(), Listen])
	end,
	loop(Listen).
	
% actual worker
worker(Server, Listen) ->
	% waits for socket
	case gen_tcp:accept(Listen) of
		% yay! got an connection
		{ok, Socket} ->
			% spawn another, por favor
			Server ! next_worker,
			echo(Socket);
		{error, Reason} ->
			Server ! next_worker,
			io:format("Can't accept socket ~p~n", [Reason])
	end.

% send back whatever is received
echo(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
			gen_tcp:send(Socket, Packet),
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Packet rejected because: ~p~n", [Reason])
	end.


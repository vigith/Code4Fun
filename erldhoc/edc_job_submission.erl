%% Purpose: Takes in the Template Module, fills in the holes and calls the job scheduler to schedule
%%          the jobs
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% TODO   : 1.1
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-02-02   | Version 1, base code

-module(edc_job_submission).

-compile(export_all).

-include("edc_common.hrl").


%% RETURN: None
%% LOGIC : Count the number of Workers and Reducers, create dummy Reducers (ie, if no reducers are 
%%         provided, create a reducer in each Worker
%%         EDCConf = {Iterators, Reducers, Lon} = {local | remote, local | remote, Lon, [ItrArgs]}
%%         Lon -> load on a node, number of iterators =:= number of workers, same iterator can be given
%%         to all the workers
%% OBJECT: call edc_worker:start_link(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf)
%%         (Conf = {in_memory | in_disk, bulk | streaming, worker count})
%% CASES :
%% 1. Iterators
%%    1.a. Local
%%    1.b. Remote (same node as Worker one-to-one)
%% 2. Worker
%%    2.a. Always Remote (else define the nodes as local)
%% 3. Sprayer
%%    3.a. No Sprayers (use a default sprayer one-to-one)
%%    3.b. Sprayers
%% 4. Reducers
%%    4.a. Local
%%    4.b. Remote
%%    4.c. No Reducers (same nodes as workers and use default workers)
start(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, JobConf, {ItLR, Lon, RdLR, ItArgs}) -> %% JobConf => {mem, disk}
    %% ask JSAGENT to return the list of nodes
    ?JSAGENT ! {self(), {get_worker_nodes, {length(Iterators), Lon, length(Reducers)}}},
    io:format("here ~w | ~w | ~w~n", [Iterators, Lon, Reducers]),
    receive
	{?JSAGENT, _, {advisable, {WNodes, RN}}} ->
	    %% if no reducers then spawn at each worker node a reducer too (local saving)
	    %% no reducers imply default sprayer (explicit by passing the default sprayer)
	    if 
		length(Reducers) =:= 0 ->
		    RNodes = WNodes;
		true ->
		    RNodes = RN
	    end;    
	{?JSAGENT, _, {notadvisable, _}} ->
	    WNodes = RNodes = [],	    
	    notadvisable,
	    io:format("ERROR: Less number of Nodes free~n"),
	    stop();
	Any ->
	    WNodes = RNodes = [],	    
	    io:format("UNKNOWN: Received Wrong Message [~w]~n", [Any]),
	    stop()
    after ?MAXWAIT ->
	    WNodes = RNodes = [],	    
	    io:format("ERROR: MAXWAIT Exceeded, no reply from Job Scheduler!~n"),
	    stop()
    end,
    %% remove the sysload part
    JstWrks   = [ Node || {Node, _} <- WNodes],
    JstRedcrs = [ Node || {Node, _} <- RNodes],
    %% spawn the reducers
    ?JSAGENT ! {self(), {spawn_reducers, {TmpMdl, Reducers}, {RdLR}, {JstRedcrs}}}, %% no reducers means default (explicit)
    RedcrPids = receive
		    {?JSAGENT, {reducer_pids, RPids}} ->
			RPids;
		    _ ->
			io:format("No Reducer Pids~n"),
			[]
		after ?MAXWAIT ->
			io:format("ERROR: [spawn_reducers] MAXWAIT Exceeded, no reply from Job Scheduler!~n"),
			[]
		end,
    io:format("Spawned Reducers [~w]~n", [RedcrPids]),
    %% spawn the workers
    JstWrksNCnt = each_node_count(JstWrks),
    io:format("Processing Nodes = [~w]~n", [JstWrksNCnt]),
    %% if iterators not Local spawn each iterator in the same node as of worker
    ?JSAGENT ! {self(), {spawn_workers, {TmpMdl, Iterators, Worker, Sprayer, OpTrfs, RedcrPids, JobConf}, {ItLR, ItArgs}, JstWrksNCnt}},
    receive
	{?JSAGENT, Status} ->
	    io:format("Worker Spawn Status [~w]~n", [Status]);
	RogueMsg ->
	    io:format("Rogue Status [~w] ~n", [RogueMsg])
    after ?MAXWAIT ->
	    io:format("ERROR: [spawn_workers] MAXWAIT Exceeded, no reply from Job Scheduler!~n"),
	    []
    end,
    done.

%% RETURN: [{Node1, Cnt} | T] Nodes with count of number of works that can be assigned to it
%% LOGIC : Process Dict
each_node_count([H|T]) ->
    case get(H) of
	undefined ->
	    put(H, 1),
	    each_node_count(T);
	Value ->
	    _ = put(H, Value + 1),
	    each_node_count(T)
    end;
each_node_count([]) ->
    erase().


%% RETURN: None
%% LOGIC : CleanUp
stop() ->
    io:format("Stopping~n").

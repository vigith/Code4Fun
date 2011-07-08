%% Purpose: Decides the mapper and the reducer hosts. It uses the node specs, nodes load etc to
%%          qualify a node as a worker (mapper, reducer) node 
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% TODO   : 1.1
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-02-01   | Version 1, base code
%%       |                    |              | TODO: 1. Add extra checking whether the any hosts 
%%       |                    |              | are given more jobs than it can take (after 1st filter)


-module(edc_job_scheduler).

-compile(export_all).

-include("edc_common.hrl").
-include("edc_node_specs.hrl").

%% RETURN: {ok, Pid} (started via supervisor)
start_link() ->
    Pid = spawn(?MODULE, scheduler, []),
    register(?JSAGENT, Pid),
    ?INFO(job_scheduler_log, "=============================================================", []),
    ?INFO(job_scheduler_log, "[~w] started with Pid [~w]", [?JSAGENT, Pid]),
    {ok, Pid}.


%% RETURN: None
scheduler() ->
    receive
	{Pid, {get_worker_nodes, Rqmnt}} ->   %% Rqmnt =:= {Dop, Lon, Redcrs}
	    ?DEBUG(job_scheduler_log, "Request for get_worker_nodes Received, [~w]", Rqmnt),
	    io:format("Request for get_worker_nodes Received, [~w]~n", [Rqmnt]),
	    Pid ! {?JSAGENT, node(), get_worker_nodes(Rqmnt)},
	    scheduler();
	stop ->
	    stopped;
	{Pid, {spawn_reducers, {TmpMdl, Reducers}, {RdLR}, {JstRedcrs}}} ->
	    Pid ! {?JSAGENT, {reducer_pids, spawn_reducer_nodes({TmpMdl, Reducers}, RdLR, JstRedcrs)}},
	    scheduler();
	{Pid, {spawn_workers, {TmpMdl, Iterators, Worker, Sprayer, OpTrfs, RedcrPids, JobConf}, {ItLR, ItArgs}, JstWrks}} -> 
	    Status = spawn_worker({TmpMdl, Iterators, Worker, Sprayer, OpTrfs, RedcrPids, JobConf}, {ItLR, ItArgs, JstWrks}),
	    Pid ! {?JSAGENT, {done, Status}},
	    scheduler();
	{Pid, _Any} ->
	    Pid ! {?JSAGENT, node(), unknown},
	    ?WARN(job_scheduler_log, "received UNKNOWN message [~w]", [_Any]),
	    io:format("WARN: Rogue Message Received, [~w]~n", [_Any]),
	    scheduler();
	TotalRogue ->
	    io:format("WARN: First Class Rogue Message Received, [~w]~n", [TotalRogue])
    end.

%% RETURN: None
spawn_worker({TmpMdl, Iterators, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk}}, {local, ItrArgs, [{JstWrk, JWrkCnt}| TJstWrk]}) ->
    {HItrs, TItrs} = lists:split(JWrkCnt, Iterators),    
    {HItrArgs, TItrArgs} = lists:split(JWrkCnt, ItrArgs),    
    ItrFuns = if
		   length(ItrArgs) =:= 0 ->
		       [ 
			 spawn_link(TmpMdl, lists:nth(Index, HItrs), []) 
			 || Index <- lists:seq(1, length(HItrs), 1) 
		       ];
		   true ->
		       [ 
			 spawn_link(TmpMdl, lists:nth(Index, HItrs), [lists:nth(Index, HItrArgs)])
			 || Index <- lists:seq(1, length(HItrs), 1) 
		       ]
	       end,
    %% edc_worker:start_link(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf)    
    spawn_link(JstWrk, edc_worker, start_link, [TmpMdl, ItrFuns, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk, JWrkCnt}]),
    spawn_worker({TmpMdl, TItrs, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk}}, {local, TItrArgs, TJstWrk});
%% base case for local
spawn_worker(_, {local, [], []}) ->
    done;
spawn_worker({TmpMdl, Iterators, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk}}, {remote, ItrArgs, [{JstWrk, JWrkCnt}| TJstWrk]}) ->
    {HItrs, TItrs} = lists:split(JWrkCnt, Iterators),    
    {HItrArgs, TItrArgs} = lists:split(JWrkCnt, ItrArgs),    
    ItrFuns = if
		   length(ItrArgs) =:= 0 ->
		       [ 
			 spawn_link(JstWrk, TmpMdl, lists:nth(Index, HItrs), []) 
			 || Index <- lists:seq(1, length(HItrs), 1) 
		       ];
		   true ->
		       [ 
			 spawn_link(JstWrk, TmpMdl, lists:nth(Index, HItrs), [lists:nth(Index, HItrArgs)])
			 || Index <- lists:seq(1, length(HItrs), 1) 
		       ]
	       end,
    %% edc_worker:start_link(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf)    
    io:format("rererer~n"),
    spawn_link(JstWrk, edc_worker, start_link, [TmpMdl, ItrFuns, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk, JWrkCnt}]),
    spawn_worker({TmpMdl, TItrs, Worker, Sprayer, OpTrfs, RedcrPids, {Mem, Bulk}}, {remote, TItrArgs, TJstWrk});
%% base case for local
spawn_worker(_, {remote, [], []}) ->
    done.
	
%% RETURN: Pids Of Reducers
%% LOGIC : spawn_link
spawn_reducer_nodes({TmpMdl, Reducers}, local, _) ->
    [ spawn_link(TmpMdl, lists:nth(Index, Reducers), []) || Index <- lists:seq(1, length(Reducers), 1) ];
spawn_reducer_nodes({TmpMdl, Reducers}, remote, RNodes) ->
    [ spawn_link(lists:nth(Index, RNodes), TmpMdl, lists:nth(Index, Reducers), []) || Index <- lists:seq(1, length(Reducers), 1) ].


%% RETURN: worker nodes [node() | [node()...]]
%% LOGIC : after get_all_nodes(), for each each node check_node_status() and get_node_specs(). Decide
%%         on the nodes after getting all nodes attributes based on Load = np * .75 - sysload * 0.5
%% In Arg: Dop is Degree of parallelism (how many instances you want to run) and
%%         Lon is Load on a node (how many instances can be run on a single node).
%%         Redcrs is number of Reducers
%% TODO  : A node can be given more job than it excepts, no checking after primary filtering
%%         (An already overloaded node won't be given more work though)
get_worker_nodes({Dop, Lon, Redcrs}) ->
    io:format("Getting Nodes~n"),
    AllNodes = get_all_nodes(),
    NodeSpec = get_node_specs(AllNodes),
    NodeDetails = [{Node, Record, check_node_status(Node)} || {Node, Record} <- NodeSpec],
    SortedNodes = sort_nodes(
		    %% Load = np * .75 - sysload * .5
		    [ 
		      {A1, (A2#nodeSpecs.np)*(0.75) + (A2#nodeSpecs.sysload)*(0.5)} 
		      || {A1, A2, A3} <- NodeDetails, A3 =/= [nodedown], length(A3) =< A2#nodeSpecs.np
		    ]
		   ),
    {Workers, Reducers} = select_nodes(SortedNodes, {Dop, Redcrs}),
    %% {advisable | notadvisable, {Workers, Reducers}}
    advisable_execution({Workers, Reducers}, Lon).


%% RETURN: {advisable | notadvisable, SelectedNodes}
%% LOGIC : count each node and see whether it exceeds the execution count
advisable_execution({Workers, Reducers}, Lon) ->
    SelectedNodes = lists:append(Workers, Reducers),
    {test_each_node_count(SelectedNodes, Lon), {Workers, Reducers}}.


%% RETURN: advisable_execution | notadvisable
%% LOGIC : Create a process dict to count
test_each_node_count([H|T], Lon) ->
    case get(H) of
	undefined ->
	    put(H, 1),
	    test_each_node_count(T, Lon);
	Lon -> %% Lon + 1 is trouble
	    erase(),
	    notadvisable;
	Value ->
	    _ = put(H, Value + 1),
	    test_each_node_count(T, Lon)
    end;
test_each_node_count([], _) ->
    erase(),
    advisable.


%% RETURN: List of Nodes eligible for running the job {Workers, Reducers}
%% LOGIC : Iterate
select_nodes(Nodes, {Dop, Redcrs}) ->
    select_nodes(Nodes, {Dop, Redcrs}, [], [], []).

select_nodes(_, {0, 0}, Workers, Reducers, _) ->
    {lists:reverse(Workers), lists:reverse(Reducers)};
select_nodes([H|T], {0, Redcrs}, Workers, Reducers, Heads) ->
    select_nodes(T, {0, Redcrs - 1}, Workers, [H | Reducers], [H | Heads]);
select_nodes([H|T], {Wrkrs, Redcrs}, Workers, Reducers, Heads) ->
    select_nodes(T, {Wrkrs - 1, Redcrs}, [H | Workers], Reducers, [H | Heads]);
%% if no hosts are passed
select_nodes([], {_, _}, _, _, []) ->
    {[], []};
%% copy the list back (circular buffer)
select_nodes([], {Wrkrs, Redcrs}, Workers, Reducers, Heads) ->
    select_nodes(lists:reverse(Heads), {Wrkrs, Redcrs}, Workers, Reducers, []).


%% RETURN: List of Nodes sorted in ascending order (smallest load first)
sort_nodes(Nodes) ->
    lists:reverse(lists:keysort(2, Nodes)). %% sort based on 2nd element (returns asc order)


%% RETURN: how many jobs are scheduled
%% LOGIC : server_call
check_node_status(Node) ->
    case rpc:server_call(Node, ?WAGENT, ?WAGENT, running_processes_details) of
	{error, _Reason} ->
	    io:format("[~w]~n", [_Reason]),
	    ?WARN(job_scheduler_log, "Node Check Failed, Reason=[~w]", [_Reason]),
	    [_Reason];
	Reply ->
	    Reply
    end.


%% RETURN: node specs (record)
%% LOGIC : multi_server_call
get_node_specs(Nodes) ->
    {Replies, BadNodes} = rpc:multi_server_call(Nodes, ?NSAGENT, get_node_specs),
    [ ?WARN(job_scheduler_log, "Bad Node [~w]", _Node) || _Node <- BadNodes ],
    Replies.


%% RETURN: the nodes the cluster is connected to [node() | [node()...]]
%% LOGIC : query the NSAGENT of the local host
get_all_nodes() ->
    ?NSAGENT ! {self(), get_cluster_nodes},
    receive
	{?NSAGENT, _, Nodes} ->
	    Nodes
    after ?MAXWAIT ->
	    exit({"NSAGENT Timed Out", ?MAXWAIT})
    end.
    

%% RETURN: None
%% LOGIC : (cleanup)
stop() ->
    ?INFO(job_scheduler_log, "stopping [~w]", [?JSAGENT]),
    ?INFO(job_scheduler_log, "=============================================================", []),
    %% stop the agent process
    ?JSAGENT ! stop,
    %% unregister the agent name
    unregister(?JSAGENT).

%% Purpose: Worker Process Head and Workers
%%          and their sub processes (each problem will have its own worker set for a node)
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% TODO   : 1.1, 1.2
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  3.   | Vigith Maurice     | 2010-02-03   | 1. Iterators are moved to Pids rather than funs
%%       |                    |              | 
%%  2.   | Vigith Maurice     | 2010-01-29   | 1. Improved the message syntax passed to Head Group Process
%%       |                    |              | 2. suffix the log4erl appender with _log
%%       |                    |              | 
%%  1.   | Vigith Maurice     | 2010-01-27   | Version 1, base code 
%%       |                    |              | As of 1.0 "Conf" contains the following tuple
%%       |                    |              | { in-memory or in-disk, bulk or streaming, worker count }
%%       |                    |              | TODO: 
%%       |                    |              | 1. Split the Internal Functions like bucket code etc to 
%%       |                    |              |    ?BASEMODULE
%%       |                    |              | 2. Fill-in the stubs
%%       |                    |              | 


-module(edc_worker).

-compile(export_all).

-include("edc_common.hrl").

%% RETURN: {'EXIT', Pid, normal} if no erlang exceptions ELSE
%%         {'EXIT', Pid, Why}
%% LOGIC : ()--> () head of pg
%%          \--> () core erldhoc worker
start_link(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf) ->
    %% validate_request won't return is args are not valid
    validate_request(TmpMdl, Iterators, Worker, OpTrfs, Sprayer, Reducers, Conf),
    ?INFO(worker_log, "Validation Successful Args=[~w | ~w | ~w | ~w | ~w | ~w | ~w]", [TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf]),
    %% unique process group identifier
    Ref = erlang:make_ref(),
    %% spawn a pg head (head process group)
    HPGPid = spawn(fun() -> head_pg([], Ref) end),
    %% Also register (and unregsiter) to the 'worker_agent'
    ?WAGENT ! {HPGPid, {Ref, register}},

    %% Start the Worker System
    spawn(?MODULE, core_elrdhoc_worker, [TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf, HPGPid]),

    %% return
    {ok, Ref, HPGPid}.


%% RETURN: None
%% LOGIC : Recurse and receive updates from the core worker and append in the list
%%         when core is done stop recursion. (also does unregister from the WORKER AGENT)
head_pg(Messages, Ref) ->
    receive
	stop ->
	    ?WAGENT ! {self(), {Ref, unregister}},
	    done;
	{Pid, retreive} ->
	    Pid ! Messages,
	    head_pg(Messages, Ref);
	Data ->
	    head_pg([Data | Messages], Ref)
    end.

%% RETURN: None
%% LOGIC : Framework execution Logic is in the Sequential Steps:
%%         Iterators + Workers -> Sprayers -> OpTrfs[repeat] -> Reducers
%%         core_elrdhoc_worker kicks off the framework
%%         (Create the Bucket Process where to Sprayer can spray, Prerequisite)
core_elrdhoc_worker(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf, HPGPid) ->
    %% create bucket process
    BktPids = create_buckets(Conf, length(Reducers)),
    io:format("Created Buckets with Pids=[~w]", [BktPids]),
    %% spawn the core erldhoc worker
    process_flag(trap_exit, true),
    EDCWPid = spawn_link(?MODULE, erldhoc_worker, [TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf, BktPids, HPGPid]),
    HPGPid ! {'S', 'EdcWrkr'},
    %% receive for EXIT status of erldhoc worker
    receive
	{'EXIT', EDCWPid, normal} ->
	    ?INFO(worker_log, "Done ErlDhoC Worker. Pid=[~w]", [EDCWPid]),
	    HPGPid ! {'D', 'EdcWrkr'},
	    %% do cleanup what-so-ever
	    cleanup(Conf, BktPids, HPGPid);
	{'EXIT', EDCWPid, Why} ->
	    ?ERR(worker_log, "Error ErlDhoC Worker. Pid=[~w] Reason=[~w]", [EDCWPid, Why]),
	    HPGPid ! {'E', 'EdcWrkr'},
	    %% do cleanup what-so-ever
	    cleanup(Conf, BktPids, HPGPid),
	    exit(Why)
    end,

    %% finished
    done.

%% RETURN: None
%% LOGIC : 
%% ======
%% Iterators + Workers -> Sprayers (=> Buckets) -> OpTrfs[repeat] (=> Senders) -> Reducers
%% High Level Data Flow Diagram
%% ----------------------------
%%  /---> () Worker1(Iterator1) --> |       |      /---> () Bucket1 -------> () O/p-Trf-1 ------> () Sender1 --------> () Reducer1
%% ()---> () ... [Wrkers + Itr] --> |SPRAYER| ==> |----> () .. [BUCKETS] --> () ... [O/p Trf] --> () ... [SENDERS] --> () ... [REDUCERS]
%%  \---> () WorkerN(IteratorN) --> |       |      \---> () BucketM -------> () O/p-TrfM -------> () SenderM --------> () ReducerM
%% count changes from N to M
%% Framework Addons
%% ----------------
%% 1. Bucket Processes (Sprayer sprays to each bucket and len(buckets) = len(Reducers)
%% 2. Sender (sends to reducer and is eaual to len(Reducers)) 
%% Code Execution Logic
%% --------------------
%% (3 stages)
%% 1. Worker Head
%%    1.a. Do the worker sequence
%%    1.b. Spray to Bucket using Sprayer
%% 2. Transformer
%% 3. Send Data to Reducers
erldhoc_worker(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, Conf, BktPids, HPGPid) ->
    %% trap and don't die
    process_flag(trap_exit, true),
    %% Stage 1
    %% spawn_link WORKER HEAD
    WHPid = spawn_link(?MODULE, worker_head, [TmpMdl, Iterators, Worker, Sprayer, Conf, BktPids, HPGPid]),
    HPGPid ! {'S' ,'WHead'},
    %% receive for EXIT status of worker head
    receive
	{'EXIT', WHPid, normal} ->
	    ?INFO(worker_log, "Done Worker Head. Pid=[~w]", [WHPid]),
	    HPGPid ! {'D', 'WHead'};
	{'EXIT', WHPid, WHwhy} ->
	    ?ERR(worker_log, "Error Worker Head. Pid=[~w] Reason=[~w]", [WHPid, WHwhy]),
	    HPGPid ! {'E', 'WHead'},
	    exit(WHwhy)
    end,
    %% Stage 2
    %% OUTPUT TRANSFORMERS
    %% generic fun for applying Output Transformer
    Instance = 
	fun(BktIndex) ->
		OPTPid = spawn_link(?MODULE, op_trf_head, [TmpMdl, lists:nth(BktIndex,OpTrfs), BktPids, Conf, HPGPid]),
		HPGPid ! {'S', 'OpTrfHead'},
		%% receive for EXIT status of Output Transformers
	    receive
		{'EXIT', OPTPid, normal} ->
		    ?INFO(worker_log, "Done Output Transformer Head. Pid=[~w]", [OPTPid]),
		    HPGPid ! {'D', 'OpTrfHead'};
		{'EXIT', OPTPid, OPTwhy} ->
		    ?ERR(worker_log, "Error Output Transformer Head. Pid=[~w] Reason=[~w]", [OPTPid, OPTwhy]),
		    HPGPid ! {'E', 'OpTrfHead'},
		    exit(OPTwhy)
	    end
	end,
    %% spawn each O/p Trf sequentially
    [ Instance(BktIndex) || BktIndex <- lists:seq(1, length(OpTrfs), 1) ],
    %% Stage 3
    %% SENDERS (send to reducers)
    SPid = spawn_link(?MODULE, sender_head, [Reducers, Conf, BktPids, HPGPid]),
    HPGPid ! {'S', 'SndrHead'},
    %% receive for EXIT status of Sender
    receive
	{'EXIT', SPid, normal} ->
	    ?INFO(worker_log, "Done Sender Head. Pid=[~w]", [SPid]),
	    HPGPid ! {'D', 'SndrHead'};
	{'EXIT', SPid, Swhy} ->
	    ?ERR(worker_log, "Error Sender Head. Pid=[~w] Reason=[~w]", [SPid, Swhy]),
	    HPGPid ! {'E', 'SndrHead'},
	    exit(Swhy)
    end,

    %% finished (done)
    done.


%% RETURN: {'EXIT', _, normal} if successful, otherwise error
%% LOGIC : spawn using list comprehension and do a sequential call to each component
%% TODO  : streaming support for data transmission from Worker -> Reducer (given, no o/p TRF)
worker_head(TmpMdl, Itrs, Wrkr, Spry, {_, bulk, WrkCnt}, BktPids, HPGPid) ->
    %% if in-streaming is set another list comprehension for spawning will be called
    %% worker listner
    WLPid = spawn(?MODULE, worker_listener, [{0, 0}, WrkCnt, self()]),
    %% spawn it in a list comprehension, each instance is independent
    [spawn(
       fun() ->
	       process_flag(trap_exit, true),
	       WPid = spawn_link(?MODULE, worker, [TmpMdl, lists:nth(Index, Itrs), Wrkr, Spry, BktPids]),
	       HPGPid ! {'S', 'Worker'},
	       receive
		   {'EXIT', WPid, normal} ->
		       HPGPid ! {'D', 'Worker'},
		       ?INFO(worker_log, "Done Worker. Pid=[~w]", [WPid]),
		       WLPid ! natural;
		   {'EXIT', WPid, Why} ->
		       WLPid ! unnatural,
		       HPGPid ! {'E', 'Worker'},
		       ?ERR(worker_log, "Error Worker. Pid=[~w] Reason=[~w]", [WPid, Why]),
		       exit(Why)
	       end
       end)
     || Index <- lists:seq(1, WrkCnt, 1) ],
	
    %% wait for the worker listener to report the end count
    receive     
	{WLPid, {WrkCnt, 0}} ->
	    done;
	{_, {SuccCnt, FailCnt}} ->
	    exit({"some workers failed", SuccCnt, FailCnt})
    end;
%% streaming code
worker_head(_, _, _, _, {_, streaming, _}, _, _) ->
    stub.


%% RETURN: send the success failure count as a tuple
worker_listener({Nat, Unnat}, WrkCnt, Pid) when Nat + Unnat < WrkCnt ->
    receive
	natural ->
	    worker_listener({Nat + 1, Unnat}, WrkCnt, Pid);
	unnatural ->
	    worker_listener({Nat, Unnat + 1}, WrkCnt, Pid)
    end;
worker_listener(Status, _, Pid) ->
    Pid ! {self(), Status}.


%% RETURN: {'EXIT', _, normal} if successful, otherwise error
%% LOGIC : Apply the conpute function (Wrkr) on the Data emitted by the 
%%         Iterator. The resultant transformed output data (TrsfmdOp) is
%%         sprayed to the buckets applying Sprayer function (Spry). Bucket PIDs
%%         are given so sprayer can send the data to different buckets.
%%         iterator return {Data, NewFun}
worker(TmpMdl, Itr, Wrkr, Spry, BktPids) ->
    Itr ! {self(), get},
    receive	
	{Itr, false} ->
	    done;
	{Itr, Data} ->
	    TrsfmdOp = apply(TmpMdl, Wrkr, [Data]),
	    ?DEBUG(worker_log, "Data = [~w] Transformed Data = [~w]",[Data, TrsfmdOp]),
	    io:format("Data = [~w] Transformed Data = [~w]~n",[Data, TrsfmdOp]),
	    if 
		%% this is to make worker also to act as a filter
		TrsfmdOp =:= erldoc_data_pass ->
		    pass;
		true ->
		    apply(TmpMdl, Spry, [TrsfmdOp, BktPids])
	    end,
	    worker(TmpMdl, Itr, Wrkr, Spry, BktPids);
	Any ->
	    io:format("Rogue Data = [~w]~n",[Any]),
	    worker(TmpMdl, Itr, Wrkr, Spry, BktPids)
    end.


%% RETURN: None
%% LOGIC : for each data in the buckets spawn a O/p Trf
op_trf_head(TmpMdl, Optrf, BktPids, {in_memory, _, _}, HPGPid) ->
    OTLPid = spawn(?MODULE, optrf_listener, [{0, 0}, length(BktPids), self()]),
    %% spawn it in a list comprehension, each instance is independent
    [spawn(
       fun() ->
	       process_flag(trap_exit, true),
	       OTPid = spawn_link(?MODULE, op_trf, [TmpMdl, Optrf, lists:nth(Index,BktPids), {in_memory}]),
	       HPGPid ! {'S', 'Op Trf'},
	       receive
		   {'EXIT', OTPid, normal} ->
		       HPGPid ! {'D', 'Op Trf'},
		       ?INFO(worker_log, "Done Op Trf. Pid=[~w]", [OTPid]),
		       OTLPid ! natural;
		   {'EXIT', OTPid, Why} ->
		       OTLPid ! unnatural,
		       HPGPid ! {'E', 'Op Trf'},
		       ?ERR(worker_log, "Error Op Trf. Pid=[~w] Reason[~w]", [OTPid, Why]),
		       exit(Why)
	       end
       end)
     || Index <- lists:seq(1, length(BktPids), 1) ],

    %% wait for the worker listener to report the end count
    LenBkts = length(BktPids),
    receive     
	{OTLPid, {LenBkts, 0}} ->
	    done;
	{_, {_, _}} ->
	    exit('some Output Transformers failed')
    end;
op_trf_head(_, _, _, {in_disk, _, _}, _) ->
    stub.


%% RETURN: send the success - failure count as a tuple
optrf_listener({Nat, Unnat}, BktCnt, Pid) when Nat + Unnat < BktCnt ->
    receive
	natural ->
	    optrf_listener({Nat + 1, Unnat}, BktCnt, Pid);
	unnatural ->
	    optrf_listener({Nat, Unnat + 1}, BktCnt, Pid)
    end;
optrf_listener(Status, _, Pid) ->
    Pid ! {self(), Status}.


%% RETURN: {'EXIT', _, normal} if successful, otherwise error
%% LOGIC : Apply the O/p Trf function on data returned by the bucket,
%%         save the data back into the bucket. (bucket get auto flushed if
%%         pull happens)
op_trf(TmpMdl, Optrf, BktPid, {in_memory}) ->
    BktPid ! {self(), get},
    receive
	Data ->
	    ?DEBUG(worker_log, "Data=[~w] from BUCKET=[~w]", [Data, BktPid]),	    
	    %% transform the Data using Transformation Function
	    TrnsfmdData = apply(TmpMdl, Optrf, [Data]),
	    %% send the data back to the bucket
	    BktPid ! {put_bulk, TrnsfmdData}
    after ?MAXWAIT ->
	    exit('(op_trf) wait timeout on bucket')
    end;
op_trf(_, _, _, {in_disk}) ->
    stub.

%% RETURN: None
%% LOGIC : for each data in the buckets send it across to Reducer and
%%         Bucket 1 always point to Reducer 1
sender_head(Redcrs, {in_memory, _, _}, BktPids, HPGPid) ->
    SLPid = spawn(?MODULE, sender_listener, [{0, 0}, length(BktPids), self()]),
    %% spawn it in a list comprehension, each instance is independent
    [spawn(
       fun() ->
	       process_flag(trap_exit, true),
	       SPid = spawn_link(?MODULE, sender, [lists:nth(Index, Redcrs), lists:nth(Index,BktPids), {in_memory}]),
	       HPGPid ! {'S', 'Sender'},
	       receive
		   {'EXIT', SPid, normal} ->
		       HPGPid ! {'D', 'Sender'},
		       ?INFO(worker_log, "Done Sender Pid=[~w]", [SPid]),
		       SLPid ! natural;
		   {'EXIT', SPid, Why} ->
		       ?ERR(worker_log, "Done Sender Pid=[~w] Reason=[~w]", [SPid, Why]),
		       HPGPid ! {'E', 'Sender'},
		       SLPid ! unnatural,
		       exit(Why)
	       end
       end)
     || Index <- lists:seq(1, length(BktPids), 1) ],

    %% wait for the worker listener to report the end count
    LenBkts = length(BktPids),
    receive     
	{SLPid, {LenBkts, 0}} ->
	    done;
	{_, {SuccCnt, FailCnt}} ->
	    exit({"some Senders failed", SuccCnt, FailCnt})
    end;
sender_head(_, {in_disk, _, _}, _, _) ->
    stub.

%% RETURN: {'EXIT', _, normal} if successful, otherwise error
%% LOGIC : pull the data from each bucket and send it to the reducer
%% TODO  : implement in_disk version
sender(Rdcr, BktPid, {in_memory}) ->
    BktPid ! {self(), get},
    receive
	Data ->
	    %% send the data to the reducer
	    %% make it a remote function call which internally stores the data
	    Rdcr ! Data
    after ?MAXWAIT ->
	    exit({"(sender) wait timeout on bucket", ?MAXWAIT})
    end;
sender(_, _, {in_disk}) ->
    stub.

%% RETURN: send the success - failure count as a tuple
sender_listener({Nat, Unnat}, BktCnt, Pid) when Nat + Unnat < BktCnt ->
    receive
	natural ->
	    sender_listener({Nat + 1, Unnat}, BktCnt, Pid);
	unnatural ->
	    sender_listener({Nat, Unnat + 1}, BktCnt, Pid)
    end;
sender_listener(Status, _, Pid) ->
    Pid ! {self(), Status}.


%% RETURN: Created Buckets' PIDs
%% LOGIC : spawn buckets equal to length(Reducers) and return the PIDs
%%         if length(Reducers) == 0, create an in_disk_bucket process
%% TODO  : in_disk_bucket code
create_buckets({in_memory, _, _}, RdcrCnt) ->
    Pids = [ spawn(?MODULE, in_mem_bucket, [[]]) || _ <- lists:seq(1, RdcrCnt, 1)],
    Pids;
create_buckets({in_disk, _, _}, RdcrCnt) ->
    Pids = [ spawn(?MODULE, in_disk_bucket, [[]]) || _ <- lists:seq(1, RdcrCnt, 1)],
    Pids;
create_buckets(_, 0) ->
    Pid = spawn(?MODULE, in_disk_bucket, [[]]),
    Pid.

%% RETURN: None
%% LOGIC : Recurse and append if new data is pushed,
%%         send data, flush and recurse if data is pulled (one-time pull)
in_mem_bucket(Data) ->
    receive
	stop ->
	    ?INFO(worker_log, "in_mem_bucket stopping!", []),
	    done;
	{line_pass, pass} ->
	    ?DEBUG(worker_log, "in_mem_bucket (put_stream) Received=[~w] Pid=[~w]", [pass, self()]),
	    in_mem_bucket(Data);
	{put_stream, Text} ->
	    ?DEBUG(worker_log, "in_mem_bucket (put_stream) Received=[~w] Pid=[~w]", [Text, self()]),
	    in_mem_bucket([Text | Data]);
	{put_bulk, Text} ->
	    ?DEBUG(worker_log, "in_mem_bucket (put_bulk) Received=[~w] Pid=[~w]", [Text, self()]),
	    in_mem_bucket(lists:append(Text, Data));
	{Pid, get} ->
	    Pid ! lists:reverse(Data),
	    in_mem_bucket([])
    end.

%% RETURN: None
%% LOGIC : Writes to a temporary file
%% TODO  : IMPLEMENTATION NOT YET DONE 
in_disk_bucket(_Data) ->
    stub.

%% RETURN: None
cleanup({in_memory, _, _}, BktPids, HPGPid) ->
    %% ask head process group to stop
    HPGPid ! stop,
    %% stop buckets
    [ Pid ! stop || Pid <- BktPids ];
cleanup({in_disk, _, _}, _, _) ->
    stub.

%% RETURN: return if worker set is valid
%%          exit() if NOT valid
%% LOGIC : len(Iterators) = len(Workers)
%%         is_function(Iterators)
%%         is_atom(Workers)
%%         is_atom(Sprayers)
%%         is_atom(OpTrfs) [if any OpTrfs]
%%         is_pid(Reducers)
validate_request(TmpMdl, Itr, Wrks, OTs, Sprs, Rdcrs, {_, _, WrksCnt}) ->
    if 
	is_atom(TmpMdl) ->
	    true;
	true ->
	    exit({"Template Module should have been a 'atom'", TmpMdl})
    end,
    if 
	length(Itr) =:= WrksCnt ->
	    true;
	true ->
	    exit({"length of Iterators NOT EQUAL to Worker Count", length(Itr), WrksCnt})
    end,
    LItr  = [ X || X <- Itr,    is_pid(X)  ],
    LWrks = [ X || X <- [Wrks], is_atom(X) ],
    LOTs  = [ X || X <- [OTs],  is_atom(X) ],
    LSprs = [ X || X <- [Sprs], is_atom(X) ],
    LRdcrs= [ X || X <- Rdcrs,  is_pid(X)  ],
    if
	length(Itr)   =/= length(LItr)
	orelse
	0 =/= length(LWrks)
	orelse
	0   =/= length(LOTs)
	orelse
	0  =/= length(LSprs)
	orelse
	length(Rdcrs) =/= length(LRdcrs) ->
	    true;
	true ->
	    exit({"Expected Function objects, Pids but received few (or all) NON Expected Objects"})
    end.

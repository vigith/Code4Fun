%% Purpose: Worker Agent is the meta-data repository of about all the worker process groups
%%          and their sub processes
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  2.   | Vigith Maurice     | 2010-02-03   | E-2-E testing code (modified version of test1.erl)
%%       |                    |              | 
%%  1.   | Vigith Maurice     | 2010-01-28   | Version 1, base code. Very basic test 


-module(test2).

-compile(export_all).

%% start(TmpMdl, Iterators, Worker, Sprayer, OpTrfs, Reducers, JobConf, {ItLR, Lon, RdLR, ItArgs})
start() ->
    edc_job_submission:start(?MODULE, [iterator1, iterator2], 
			     worker, sprayer, [opTrf1, opTrf2, opTrf3], [reducer1, reducer2], 
			     {in_memory, bulk},
			     {remote, 2, local, [0, 10]}).

iterator1(N) -> 
    receive 
	{Pid, get} ->
	    if 
		N < 10 ->
		    Pid ! {self(), N},
		    iterator1(N+1);
		true ->
		    Pid ! {self(), false}
	    end;
	{Pid, stop} ->
	    Pid ! stop
    end.

iterator2(N) -> 
    receive 
	{Pid, get} ->
	    if 
		N < 20 ->
		    Pid ! {self(), N},
		    iterator2(N+1);
		true ->
		    Pid ! {self(), false}
	    end;
	{Pid, stop} ->
	    Pid ! stop
    end.


worker(Data) ->
    Data * 3.

sprayer(Op, [Bkt1, Bkt2]) ->
    if
	Op rem 2 =:= 0 ->
	    io:format("Sprayer [~w] to Bkt [~w]~n", [Op, Bkt1]),
	    Bkt1 ! {put_stream, Op};
	true ->
	    io:format("Sprayer [~w] to Bkt [~w]~n", [Op, Bkt2]),
	    Bkt2 ! {put_stream, Op}
    end.

opTrf1(Data) ->
    [X + 1 || X <- Data].

opTrf2(Data) ->
    [X + 2 || X <- Data].

opTrf3(Data) ->
    [X - 2 || X <- Data].

reducer1() ->
    receive
	In ->
	    io:format("One [~w]~n", [In]),
	    done,
	    reducer1()
    end.

reducer2() ->
    receive
	In ->
	    io:format("Two [~w]~n", [In]),
	    done,
	    reducer2()
    end.

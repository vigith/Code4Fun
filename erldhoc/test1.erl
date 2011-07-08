%% Purpose: Worker Agent is the meta-data repository of about all the worker process groups
%%          and their sub processes
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-28   | Version 1, base code. Very basic test 


-module(test1).

-compile(export_all).

start() ->
    Pid1 = spawn(fun() -> reducer1() end),
    Pid2 = spawn(fun() -> reducer2() end),
    {_, Itr1} = iterator1(-1),
    {_, Itr2} = iterator2(10),
    edc_worker:start_link(?MODULE, [Itr1, Itr2], worker, sprayer, [opTrf1, opTrf2], [Pid1, Pid2], {in_memory, bulk, 2}).

iterator1(N) -> 
    { N, fun() -> 
		   if 
		       N < 10 ->
			   iterator1(N+1);
		       true ->
			   false
		   end 
	   end
    }.

iterator2(N) -> 
    { N, fun() -> 
		   if 
		       N < 20 ->
			   iterator2(N+1);
		       true ->
			   false
		   end 
	   end
    }.

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

reducer1() ->
    receive
	In ->
	    io:format("One [~w]~n", [In]),
	    reducer1()
    end.

reducer2() ->
    receive
	In ->
	    io:format("Two [~w]~n", [In]),
	    reducer2()
    end.

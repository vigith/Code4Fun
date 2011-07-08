%% Purpose: Worker Agent is the meta-data repository of about all the worker process groups
%%          and their sub processes
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-25   | Version 1, base code

-module(edc_worker_agent).

-compile(export_all).

-include("edc_common.hrl").

%% RETURN: {ok, Pid} 'coz this return is mandatory for "supervisor"
start_link() ->
    Pid = spawn_link(?MODULE, listener, []),
    register(?WAGENT, Pid),
    ?INFO(worker_agent_log, "=============================================================", []),
    ?INFO(worker_agent_log, "[~w] started with Pid [~w]", [?WAGENT, Pid]),
    {ok, Pid}.


%% RETURN: None (server)
%% LOGIC : Recurses on same function and dispatches
%%         the corresponding function for each message
listener() ->
    receive	
	{Pid, {Pg_id, register}} ->
	    pg_register(Pg_id, Pid),
	    listener();
	{Pid, {Pg_id, unregister}} ->
	    pg_unregister(Pg_id, Pid),
	    listener();
	{Pid, running_processes_details} ->
	    Pid ! {?WAGENT, node(), running_processes_details()},
	    listener();
	{Pid, purge} ->
	    purge(Pid),
	    listener();
	{_Pid, stop} ->	    
	    stop();
	_Any ->
	    ?WARN(worker_agent_log, "[~w] got UNKOWN message [~w]", [?WAGENT, _Any]),
	    listener()
    end.

%% RETURN: None
pg_register(Pid, Pg_id) ->
    Time = systemtime(),
    put(Pg_id, {Pid, Time}),
    ?INFO(worker_agent_log, "Added [~w] to Process Dictionary with Key [~w] and Time [~w]", [Pid, Pg_id, Time]).

%% RETURN: None
pg_unregister(Pg_id, _Pid) ->
    %% there is no checking whether the process who asked to erase the entry is 
    %% same as the one who got registered himself because JT can ask a pg to get
    %% removed if it sees that the pg is down and it can no longer ask by itself to 
    %% get itself removed
    erase(Pg_id),
    ?INFO(worker_agent_log, "Erased from Process Dictionary the Key [~w] and Request Pid is [~w]", [Pg_id, _Pid]).

%% RETURN: None (graceful shutdown)
stop() ->
    ?INFO(worker_agent_log, "[~w] stopping", [?WAGENT]),
    ?INFO(worker_agent_log, "=============================================================", []),
    unregister(?WAGENT),
    stopped.

%% RETURN: None
%% LOGIC : Remove the Processes inside process dict which is older than X hours
%%         Current - Start time > PROCESS_CLEAN_TIMEOUT
purge(_Pid) ->
    ?INFO(worker_agent_log, "Request to Purge from Pid [~w]", [_Pid]),
    Time = systemtime(),
    Victims = [ X || X = {_, {_, StartTime}} <- get(), Time - StartTime >= ?PROCESS_CLEAN_TIMEOUT],
    delete_victims(Victims).

%% TODO
%% 1. Add an option to heart beat the Pid before deleting
delete_victims([]) ->
    deleted;
delete_victims([{Id, {_Pid, _St}} | T]) ->
    ?DEBUG(worker_agent_log, "removing Id [~w] with Start Time [~w] from Process Dictionary", [Id, _St]),
    erase(Id),
    delete_victims(T).


%% RETURN: process dictionary get()
%% TODO  :1. Iteratively get each process detail from the Worker Head of a PG (process group)
running_processes_details() ->
    ?DEBUG(worker_agent_log, "Request from Pid [~w] for running_process_details", [Pid]),
    get().


%% RETURN: localtime {_, Sec, _}
%% TODO  : May be we have to port it to UTC time so this function is a
%%         strategy
systemtime() ->
    {_, Sec, _} = erlang:now(), % {Megaseconds,Seconds,Microseconds}
    Sec.

%% Purpose: All the functions related to system level details such as diskspace,
%%          memory utillization, load average etc.
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-25   | Version 1, base code

-module(edc_host_details).

-compile(export_all).

%% RETURN: disk space list
get_disk_space() ->
    disksup:get_disk_data().

%% RETURN: system memory data
get_sys_memory() ->
    memsup:get_system_memory_data().

%% RETURN: number of processes running
get_process_count() ->
    cpu_sup:nprocs().

%% RETURN: load average in last 5 mins 
get_load_avg() ->
    cpu_sup:avg5().

%% RETURN: CPU utilization
get_cpu_util() ->
    try cpu_sup:util() of
	X -> X
    catch 
	_:_ ->
	    0
    end.

%% RETURN : boolean 1 if the application (eg, os_mon) is loaded
%%          boolean 0 if the application is NOT loaded
is_loaded(Appl) ->
    is_loaded(Appl, [ A || {A, _, _} <- application:which_applications() ]).

is_loaded(Appl, [Appl | _]) ->
    1;
is_loaded(Appl, [_| T]) ->
    is_loaded(Appl, T);
is_loaded(_, []) ->
    0.

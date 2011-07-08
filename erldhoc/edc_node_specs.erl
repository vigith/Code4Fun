%% Purpose: Node Specification Process, stand alone, responses to the node spec request
%%          (reads from node_conf.hrl)
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% TODO   : 1.1, 1.2
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-30   | Version 1, base code (start, stop, !update)
%%       |                    |              | TODO: 
%%       |                    |              | 1. include update option for every key
%%       |                    |              | 2. Save to file option
%%       |                    |              | 3. Add options to send each record item (if rqd)


-module(edc_node_specs).

-compile(export_all).

-include("edc_node_specs.hrl").
-include("edc_common.hrl").


start_link() ->
    %% read the nodeSpec record
    Record = #nodeSpecs{},
    %% spawn_link a process that reponses with the node spec
    Pid = spawn_link(?MODULE, node_spec, [Record]),
    %% register the node spec process
    register(?NSAGENT, Pid),
    ?INFO(node_spec_log, "=============================================================", []),
    ?INFO(node_spec_log, "starting [~w] with Pid=[~w]", [?NSAGENT, Pid]),
    {ok, Pid}.

%% RETURN: None
node_spec(Record) ->
    receive
	{Pid, get_node_specs} ->
	    % returns the whole record
	    Pid ! {?NSAGENT, node(), {node(), Record}},
	    node_spec(Record);
	{Pid, get_cluster_nodes} ->
	    Pid ! {?NSAGENT, node(), Record#nodeSpecs.nodes},
	    node_spec(Record);
	{Pid, {update, np, Value}} ->
	    TmpRcrd = Record#nodeSpecs{np=Value},
	    Pid ! {?NSAGENT, node(), TmpRcrd},
	    node_spec(TmpRcrd);
	stop ->
	    stopped;
	{Pid, _Any} ->
	    Pid ! {?NSAGENT, node(), unknown},
	    ?WARN(node_spec_log, "received UNKNOWN message [~w]", [_Any]),
	    node_spec(Record)
    end.

%% RETURN: None
stop() ->
    ?INFO(node_spec_log, "stopping [~w]", [?NSAGENT]),
    ?INFO(node_spec_log, "=============================================================", []),
    %% stop the agent process
    ?NSAGENT ! stop,
    %% unregister the agent name
    unregister(?NSAGENT).

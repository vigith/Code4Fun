%% Purpose: DEBUG Macros,
%%          CONSTANTS, etc
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-26   | Version 1, base code

%% DEBUG OPTIONS
-ifdef(debug).
-define(DEBUG(Logger, Text, Exp),
        log4erl:debug(Logger, Text, Exp)).
-define(INFO(Logger, Text, Exp),
        log4erl:info(Logger, Text, Exp)).
-define(WARN(Logger, Text, Exp),
        log4erl:warn(Logger, Text, Exp)).
-define(ERR(Logger, Text, Exp),
        log4erl:error(Logger, Text, Exp)).
-else.
-define(DEBUG(Logger, Text, Exp),true).
-define(INFO(Logger, Text, Exp), true).
-define(WARN(Logger, Text, Exp), true).
-define(ERR(Logger, Text, Exp), true).
-endif.

-define(PROCESS_CLEAN_TIMEOUT, 5 * 3600). % 5 hours

%% NAMED PROCESSES
%% worker agent (WAGENT)
-define(WAGENT, 'worker_agent').
%% node spec agent (NSAGENT)
-define(NSAGENT, 'node_spec_agent').
%% cluster node agent (CNAGENT)
-define(CNAGENT, 'cluster_nodes_agent') .
%% job scheduler (JSAGENT)
-define(JSAGENT, 'job_scheduler_agent').

%% CONSTANTS
-define(MAXWAIT, 1000). % 1,000 milliseconds

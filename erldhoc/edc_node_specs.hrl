%% Purpose: Node Spec Configuration File
%%          (used by edc_node_specs.erl)
%% Author : Vigith Maurice (vigith@gmail.com)
%% Version: 1.0
%% License: GPL
%% TODO   : 1.1
%% Changes:
%% Sl No |        Who         |     When     |          Why        
%% =========================================================================================
%%  1.   | Vigith Maurice     | 2010-01-30   | Version 1, base code
%%       |                    |              | TODO: 1. Add more specs into conf

-record(nodeSpecs, 
	{
	  %% number of processes a node can take parallel
	  np = 4,
	  %% sysload (usual system load due to other process)
	  %% used by job tracker to skip nodes with higher count for job scheduling
	  sysload = 2,
	  %% node name
	  name = 'daremudfill-lm',
	  %% hostname
	  hname = 'node3',
	  %% cluster hosts
	  nodes = [
		   'node1@daremudfill-lm',
		   'node2@daremudfill-lm'
		  ]
	}
       ).

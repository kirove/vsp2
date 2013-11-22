%%%-------------------------------------------------------------------
%%% @author Akcicek, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2013 1:34 PM
%%%-------------------------------------------------------------------
-module(node).
-author("Akcicek, Berngruber, Osman").

%% API
-export([start/0, wakeUp/3]).
-import(werkzeug, [get_config_value/2, logging/2]).
-import(logging, [start/1]).

-include_lib("Datastructure.hrl").

%% -record(node, {
%%   name,
%%   state = sleeping,
%%   find_count = undefined,
%%   fragment_id = undefined,
%%   level = undefined
%% }).
%%
%% -record(edge_management, {
%%   edge_list,
%%   in_branch_edge = undefined,
%%   best_edge = undefined,
%%   best_weight = undefined,
%%   test_edge = undefined
%% }).
%%
%% -record(edge, {
%%   weight_id,
%%   node_pid,
%%   state = basic
%% }).

start() ->

  % Read out the config file.
  {ok, ConfigListe} = file:consult("node.cfg"),
  {ok, NodeName} = get_config_value(nodename, ConfigListe),
  {ok, StartUpTime} = get_config_value(startuptime, ConfigListe),
  {ok, EdgeList} = get_config_value(edgeList, ConfigListe),
  {ok, NodeEnvironmentList} = file:consult("hosts"),


  % LogFile
  {ok, LoggingHost} = inet:gethostname(),
  % creates filename for the flogging file (append, like string-append)
  ServerLogFileName = lists:append(["Node", LoggingHost, ".log"]),
  % start logging module
  LoggingPID = spawn(fun() -> logging:start(ServerLogFileName) end),
  LoggingPID ! {start},


  % ping all NodeHosts
  SuccessfullyPingedHostList = net_adm:world_list(NodeEnvironmentList),

  LoggingPID ! {list, "NodeEnvironmentList", NodeEnvironmentList},
  LoggingPID ! {list, "SuccessfullyPingedHostList", SuccessfullyPingedHostList},

  % Register the node.
  % node gets registered as the alias
  global:register_name(NodeName, self()),

  % sleep, take time to start the other nodes
  timer:sleep(StartUpTime),


  LoggingPID ! {pid, ["Knoten", self()]},

  % RandomTime in Seconds
  RandomTimeSeconds = random:uniform(15),

  % Start wakeup-Timer for node
  {_, WakeUpTimer} = timer:send_after(timer:seconds(RandomTimeSeconds), self(), {wake_up_msg}),

  NodeStates = #node{
    name = NodeName,
    state = sleeping,
    find_count = undefined,
    fragment_id = undefined,
    level = undefined
  },

  EdgeRecordList = utilities:setUpEdgeList(EdgeList, []),


  EdgeManagement = #edge_management{
    edge_list = EdgeRecordList,
    in_branch_edge = undefined,
    best_edge = undefined,
    best_weight = infinity,
    test_edge = undefined
  },

  % start mainLoop
  mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
.

%%%%%%%%%%%%%%%%%Start Main Recive Loop%%%%%%%%%%%%%%%%%
mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID) ->
	receive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%		Recieve Wake_up_msg - by the Timer			%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {wake_up_msg} ->
		if
			% wakeup muss aufgerufen werden wenn knoten noch am schlafen ist
			NodeStates#node.state == sleeping ->
			 NewNodeStates = wakeUp(NodeStates, EdgeManagement, LoggingPID),
        mainLoop(NewNodeStates, EdgeManagement, WakeUpTimer, LoggingPID);
		% Nodestate != sleeping
		true ->
      ok,
      mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)

		end
;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve CONNECT Message					%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {connect, InboundLevel, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> connect..."},
		
		if
			% wakeup muss aufgerufen werden wenn knoten noch am schlafen ist
			NodeStates#node.state == sleeping ->
				timer:cancel(WakeUpTimer),
			 NewNodeStates =	wakeUp(NodeStates, EdgeManagement, LoggingPID);
			% Nodestate != sleeping
			true ->
				ok,
        NewNodeStates = NodeStates
		end,
		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),
		
		if
			InboundLevel < NewNodeStates#node.level ->
					SuccessfullyChanged = utilities:change_edge_state(EdgeManagement#edge_management.edge_list, InboundEdge, branch),
					if 
						not SuccessfullyChanged ->
							LoggingPID ! {output_functions, "Edge Not Found in WakeUP Procedure....."};
						true ->
							ok
					end,
					
					Edge#edge.node_pid ! {initiate, NewNodeStates#node.level, NewNodeStates#node.fragment_id, NewNodeStates#node.state, Edge},
					if
            NewNodeStates#node.state == find ->
              NewNodeStatesV2 = NewNodeStates#node{find_count = (NewNodeStates#node.find_count +1)};
						true ->
						  ok,

              NewNodeStatesV2 = NewNodeStates
					end;
			% InboundLevel >= NodeLevel
			true ->
				if
					Edge#edge.state == basic ->
						self() ! {connect, InboundLevel, Edge};
					% EdgeState != basic
					true ->
						Edge#edge.node_pid ! {initiate, NewNodeStates#node.level + 1, Edge#edge.weight_id, find, Edge}
				end     ,
        NewNodeStatesV2 = NewNodeStates
		end,
		mainLoop(NewNodeStatesV2, EdgeManagement, WakeUpTimer, LoggingPID)
	;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve INITIATE Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {initiate, InboundNodeLevel, InboundFragmentId, InboundState, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> initiate..."},
		
		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

    %setting the nodestates
		NewNodeStates = NodeStates#node{
			state = InboundState,
			fragment_id = InboundFragmentId,
			level = InboundNodeLevel
		},

    % setting the edgeValues
		EdgeManagement#edge_management{
			in_branch_edge = Edge,
			best_edge = undefined,
			best_weight = infinity,
			test_edge = undefined
		},

		 LoggingPID ! {output_receive, "in mainLoop -> initiate/branch_list_without_unwanted_edge..."},
		BranchListWithoutUnwantedEdge = utilities:get_branch_edges_without(EdgeManagement#edge_management.edge_list, Edge),
         
		lists:foreach(fun(Edge) ->
			Edge#edge.node_pid ! {initiate, InboundNodeLevel, InboundFragmentId, InboundState, Edge},
			if
				InboundState == find ->
				NodeStates#node{ find_count = NodeStates#node.find_count + 1};
				true ->
					ok

		BranchListWithoutUnwantedEdge),

		if
			InboundState == find ->
				test(NodeStates, EdgeManagement, LoggingPID);
			true ->
				ok
		end,
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve TEST Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {test, InboundLevel, InboundFragmentID, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> test..."},

		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

		if
			% wakeup muss aufgerufen werden wenn knoten noch am schlafen ist
			NodeStates#node.state == sleeping ->
				timer:cancel(WakeUpTimer),
				wakeUp(NodeStates, EdgeManagement, LoggingPID);
			% NodeState != sleeping
			true ->
				ok
		end,
		
		if
			InboundLevel > NodeStates#node.level ->
				self() ! {test, InboundLevel, InboundFragmentID, Edge};

			%InboundLevel <= NodeLevel
			true ->
				if
					not InboundFragmentID == NodeStates#node.fragment_id ->
						Edge#edge.node_pid ! {accept, Edge};

					%  InboundFragmentID == FragmentID
					true ->
						if
							Edge#edge.state == basic ->
								utilities:change_edge_state(EdgeManagement#edge_management.edge_list, Edge, rejected);
							% EdgeState != basic
							true ->
								ok
						end,
						%% Get TestEdge
						TestEdge = EdgeManagement#edge_management.test_edge,
						if
							not  Edge#edge.weight_id == TestEdge#edge.weight_id ->
								Edge#edge.node_pid ! {reject, Edge};
							% EdgeWeight_ID ==  TestEdgeID
							true ->
								test(NodeStates, EdgeManagement, LoggingPID)
						end
				end
		end,
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Accept Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {accept, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> accept..."},

		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

		%% Set Test Edge undefined
		EdgeManagement#edge_management{ test_edge = undefined},
		%% Get BestEdge
		BestEdge = EdgeManagement#edge_management.best_edge,
		
		if
            BestEdge#edge.weight_id < EdgeManagement#edge_management.best_weight ->
				EdgeManagement#edge_management{ 
					best_edge = Edge,
					best_weight = Edge#edge.weight_id
					};
			% EdgeWeight_ID >= BestWeight
			true ->
				ok
        end,
        %% Call Report Function
		report(NodeStates, EdgeManagement, LoggingPID),
		
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Reject Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {reject, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> reject/..."},
	  
		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),
		if
			Edge#edge.state == basic ->
				utilities:change_edge_state(EdgeManagement#edge_management.edge_list, Edge, rejected);
            %% if EdgeState != basic
			true ->
				ok
        end,
        test(NodeStates, EdgeManagement, LoggingPID),
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Report Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {report, InboundWeight, InboundEdge} ->
		LoggingPID ! {output_receive, "in mainLoop -> report..."},
		
		%% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
		Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),
      
		%% Get InBranchEdge
		InBranchEdge = EdgeManagement#edge_management.in_branch_edge,
        EqualEdges = utilities:proveEqual(InBranchEdge, Edge),
		if
			not EqualEdges->
				NodeStates#node{ find_count = NodeStates#node.find_count -1 },
			  
                if
                    InboundWeight < EdgeManagement#edge_management.best_weight ->
						EdgeManagement#edge_management{
							best_edge = Edge,
							best_weight = InboundWeight
						};
					%InboundWeight >= BestWeight
                    true ->
						ok
                end,
                report(NodeStates, EdgeManagement, LoggingPID);

			%  InBranchEdge == Edge
            true ->
				if
                    NodeStates#node.state == find ->
						self() ! {report, InboundWeight, Edge};

					%NodeState != find
                    true ->
						if
                            (InboundWeight > EdgeManagement#edge_management.best_weight) ->
								changeroot(NodeStates, EdgeManagement, LoggingPID);

							%InboundWeight <= BestWeight
                            true ->
								IsInboundWeightInfinite = InboundWeight == infinity,
								if
									IsInboundWeightInfinite and EdgeManagement#edge_management.best_weight == infinity ->
										haltNow(LoggingPID);
									%% IsInboundWeightInfinite or BestWeight != infinity
									true ->
										ok
								end
                        end
                end
        end,
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve ChangeRoot Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {changeroot, _} ->
		LoggingPID ! {output_receive, "in mainLoop -> changeroot..."},
		%% Call the changeroot function
		changeroot(NodeStates, EdgeManagement, LoggingPID),
		mainLoop(NodeStates, EdgeManagement, WakeUpTimer, LoggingPID)
	  
	end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%Help Functiones%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wakeUp(NodeStates, EdgeManagement, LoggingPID) ->
	LoggingPID ! {output_functions, "in wakeUp..."},

	MinEdge = utilities:getMinEdge(EdgeManagement#edge_management.edge_list),

	%% HOW TO change edgeRecord state in a list of records!!
	SuccessfullyChanged = utilities:change_edge_state(EdgeManagement#edge_management.edge_list, MinEdge, branch),
	if 
		not SuccessfullyChanged ->
				LoggingPID ! {output_functions, "Edge Not Found in WakeUP Procedure....."};
		true ->
			ok
	end,

	%% setting node new properties 
	NewNodeStates = NodeStates#node{
		state = found,
		level = 0,
		find_count = 0
	},

	% send connect other Node behind MinEdge
	MinEdge#edge.node_pid ! {connect, 0, MinEdge},
	LoggingPID ! {send_connect_to, MinEdge#edge.node_pid} ,
  NewNodeStates
.

changeroot(NodeStates, EdgeManagement, LoggingPID) ->
	LoggingPID ! {output_functions, "in changeroot()..."},
	
	%% Get Best Edge
	BestEdge = EdgeManagement#edge_management.best_edge,
	
    if
        BestEdge#edge.state == branch ->
				BestEdge#edge.node_pid ! {changeroot, BestEdge};
				
		%  BestEdgeState == branch
		true ->
			BestEdge#edge.node_pid ! {connect, NodeStates#node.level, BestEdge},
			BestEdge#edge{ state = branch}
    end
.

report(NodeStates, EdgeManagement, LoggingPID) ->
	LoggingPID ! {output_functions, "in report() ..."},
	if
		NodeStates#node.find_count == 0 ->
			if
				EdgeManagement#edge_management.test_edge == undefined ->
					%% Set Node State
					NodeStates#node{ state = found },
					%% Get InBranchEdge
					InBranchEdge = EdgeManagement#edge_management.in_branch_edge,
					%% Send Report Message on InBranchEdge
					InBranchEdge#edge.node_pid ! {report, EdgeManagement#edge_management.best_weight, InBranchEdge};
				% TestEdge != undefined
				true ->
					ok
			end;
		% FindCount != 0
		true ->
			ok
	end
.

test(NodeStates, EdgeManagement, LoggingPID) ->
	LoggingPID ! {output_functions, "in test()..."},
	MinBasicEdge = utilities:get_min_basic_edge(EdgeManagement#edge_management.edge_list),

	if
		not MinBasicEdge#edge.weight_id == undefined ->
			%% set Test Edge
			EdgeManagement#edge_management{ test_edge = MinBasicEdge },
			
			%% send Test Message on MinBasicEdge
			MinBasicEdge#edge.node_pid ! {test, NodeStates#node.level, NodeStates#node.fragment_id, MinBasicEdge};
			  
		%  MinBasicEdge == undefined
		true ->
			%% set Test Edge
			EdgeManagement#edge_management{ test_edge = undefined },
			report(NodeStates, EdgeManagement, LoggingPID)
	end
.

haltNow(LoggingPID) ->
  LoggingPID ! {output_functions, "in haltNow()..."},
  exit(LoggingPID, "LoggingPID shutdown"),
  exit(self(), "knoten_logik shutdown")
.

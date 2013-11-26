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
  LoggingPID ! {output_receive, "in mainLoop"},
  receive
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%		Recieve Wake_up_msg - by the Timer			%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {wake_up_msg} ->
      LoggingPID ! {output_receive, "in mainLoop -> wake_up_msg..."},
      if
      % wakeup muss aufgerufen werden wenn knoten noch am schlafen ist
        NodeStates#node.state == sleeping ->
          {NewNodeStates, NewEdgeManagement} = wakeUp(NodeStates, EdgeManagement, LoggingPID),

          mainLoop(NewNodeStates, NewEdgeManagement, WakeUpTimer, LoggingPID);
      % Nodestate != sleeping
        true ->
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
          {NewNodeStates, NewEdgeManagement} = wakeUp(NodeStates, EdgeManagement, LoggingPID);
      % Nodestate != sleeping
        true ->
          NewNodeStates = NodeStates,
          NewEdgeManagement = EdgeManagement
      end,
      %% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
      Edge = utilities:get_edge(NewEdgeManagement#edge_management.edge_list, InboundEdge),

      if
        InboundLevel < NewNodeStates#node.level ->
          EdgesList = utilities:change_edge_state(NewEdgeManagement#edge_management.edge_list, InboundEdge, branch),
          NewEdgeManagementV2 = NewEdgeManagement#edge_management{edge_list = EdgesList},

          LoggingPID ! {output_send_to_node, "initiate auf (in connect1)", Edge#edge.node_pid} ,
          %% send Initiate Message
          Edge#edge.node_pid ! {initiate, NewNodeStates#node.level, NewNodeStates#node.fragment_id, NewNodeStates#node.state, Edge},
          if
            NewNodeStates#node.state == find ->
              NewNodeStatesV2 = NewNodeStates#node{find_count = (NewNodeStates#node.find_count + 1)};
            true ->
              NewNodeStatesV2 = NewNodeStates
          end;
      % InboundLevel >= NodeLevel
        true ->

          if
            Edge#edge.state == basic ->
              LoggingPID ! {output_send_to_node, "requeing connect (in connect)", self()} ,
              self() ! {connect, InboundLevel, InboundEdge};
          % EdgeState != basic
            true ->
              LoggingPID ! {output_send_to_node, "initiate (in connect2)", Edge#edge.node_pid} ,
              Edge#edge.node_pid ! {initiate, NewNodeStates#node.level + 1, Edge#edge.weight_id, find, Edge}
          end,
          NewNodeStatesV2 = NewNodeStates,
          NewEdgeManagementV2 = NewEdgeManagement
      end,
      mainLoop(NewNodeStatesV2, NewEdgeManagementV2, WakeUpTimer, LoggingPID)
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
      NewEdgeManagement = EdgeManagement#edge_management{
        in_branch_edge = Edge,
        best_edge = undefined,
        best_weight = infinity
      },


      BranchListWithoutUnwantedEdge = utilities:get_branch_edges_without(NewEdgeManagement#edge_management.edge_list, Edge),

      NewNodeStatesV2 = recursiveForEachForInitiate(BranchListWithoutUnwantedEdge, NewNodeStates, InboundNodeLevel, InboundFragmentId, InboundState, LoggingPID),

      if
        InboundState == find ->
          {NewNodeStatesV3, NewEdgeManagementV2} = test(NewNodeStatesV2, NewEdgeManagement, LoggingPID);
        true ->
          NewNodeStatesV3 = NewNodeStatesV2,
          NewEdgeManagementV2 = NewEdgeManagement
      end,
      mainLoop(NewNodeStatesV3, NewEdgeManagementV2, WakeUpTimer, LoggingPID)
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
          {NewNodeStates, NewEdgeManagement} = wakeUp(NodeStates, EdgeManagement, LoggingPID);
      % NodeState != sleeping
        true ->
          NewNodeStates = NodeStates,
          NewEdgeManagement = EdgeManagement
      end,

      if
        InboundLevel > NewNodeStates#node.level ->
          LoggingPID ! {output_send_to_node, "requeing test (in test)", self()} ,
          self() ! {test, InboundLevel, InboundFragmentID, InboundEdge},
          NewNodeStatesV2 = NewNodeStates,
          NewEdgeManagementV3 = NewEdgeManagement;

      %InboundLevel <= NodeLevel
        true ->
          if
            not InboundFragmentID == NewNodeStates#node.fragment_id ->
              LoggingPID ! {output_send_to_node, "accept (in test)", Edge#edge.node_pid} ,
              Edge#edge.node_pid ! {accept, Edge},
              NewNodeStatesV2 = NewNodeStates,
              NewEdgeManagementV3 = NewEdgeManagement;
          %  InboundFragmentID == FragmentID
            true ->
              if
                Edge#edge.state == basic ->
                  EdgesList = utilities:change_edge_state(NewEdgeManagement#edge_management.edge_list, Edge, rejected),
                  NewEdgeManagementV2 = NewEdgeManagement#edge_management{edge_list = EdgesList};
              % EdgeState != basic
                true ->
                  NewEdgeManagementV2 = NewEdgeManagement
              end,
              %% Get TestEdge
              TestEdge = NewEdgeManagementV2#edge_management.test_edge,
              if
                not  Edge#edge.weight_id == TestEdge#edge.weight_id ->
                  LoggingPID ! {output_send_to_node, "reject (in test)", Edge#edge.node_pid} ,
                  Edge#edge.node_pid ! {reject, Edge},
                  NewNodeStatesV2 = NewNodeStates,
                  NewEdgeManagementV3 = NewEdgeManagementV2;
              % EdgeWeight_ID ==  TestEdgeID
                true ->
                  {NewNodeStatesV2, NewEdgeManagementV3} = test(NewNodeStates, NewEdgeManagementV2, LoggingPID)
              end
          end
      end,
      mainLoop(NewNodeStatesV2, NewEdgeManagementV3, WakeUpTimer, LoggingPID)
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Accept Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {accept, InboundEdge} ->
      LoggingPID ! {output_receive, "in mainLoop -> accept..."},

      %% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
      Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

      %% Set Test Edge undefined
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = undefined},
      %% Get BestEdge
      BestEdge = NewEdgeManagement#edge_management.best_edge,

      if
        BestEdge#edge.weight_id < NewEdgeManagement#edge_management.best_weight ->
          NewEdgeManagementV2 = NewEdgeManagement#edge_management{
            best_edge = Edge,
            best_weight = Edge#edge.weight_id
          };
      % EdgeWeight_ID >= BestWeight
        true ->
          NewEdgeManagementV2 = NewEdgeManagement
      end,
      %% Call Report Function
      NewNodeStates = report(NodeStates, NewEdgeManagementV2, LoggingPID),

      mainLoop(NewNodeStates, NewEdgeManagementV2, WakeUpTimer, LoggingPID)
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
          EdgesList = utilities:change_edge_state(EdgeManagement#edge_management.edge_list, Edge, rejected),
          NewEdgeManagement = EdgeManagement#edge_management{edge_list = EdgesList};
      %% if EdgeState != basic
        true ->
          NewEdgeManagement = EdgeManagement
      end,
      {NewNodeStates, NewEdgeManagementV2} = test(NodeStates, NewEdgeManagement, LoggingPID),
      mainLoop(NewNodeStates, NewEdgeManagementV2, WakeUpTimer, LoggingPID)
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
        not EqualEdges ->
          NewNodeStates = NodeStates#node{find_count = NodeStates#node.find_count - 1},

          if
            InboundWeight < EdgeManagement#edge_management.best_weight ->
              NewEdgeManagement = EdgeManagement#edge_management{
                best_edge = Edge,
                best_weight = InboundWeight
              };
          %InboundWeight >= BestWeight
            true ->
              NewEdgeManagement = EdgeManagement
          end,
          NewNodeStatesV2 = report(NewNodeStates, NewEdgeManagement, LoggingPID);

      % InBranchEdge == Edge
        true ->
          if
            NodeStates#node.state == find ->
              LoggingPID ! {output_send_to_node, "requeueing report (in report)", self()} ,
              self() ! {report, InboundWeight, InboundEdge},
              NewEdgeManagement = EdgeManagement;
          %NodeState != find
            true ->
              if
                (InboundWeight > EdgeManagement#edge_management.best_weight) ->
                  NewEdgeManagement = changeroot(NodeStates, EdgeManagement, LoggingPID);

              %InboundWeight <= BestWeight
                true ->

                  if
                    (InboundWeight == infinity) and (EdgeManagement#edge_management.best_weight == infinity) ->

                      NewEdgeManagement = EdgeManagement,

                    haltNow(NodeStates#node.name, EdgeManagement#edge_management.in_branch_edge#edge.weight_id ,LoggingPID);


                  %% IsInboundWeightInfinite or BestWeight != infinity
                    true ->
                      NewEdgeManagement = EdgeManagement
                  end

              end
          end,
          NewNodeStatesV2 = NodeStates
      end,
      mainLoop(NewNodeStatesV2, NewEdgeManagement, WakeUpTimer, LoggingPID)
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve ChangeRoot Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {changeroot, _} ->
      LoggingPID ! {output_receive, "in mainLoop -> changeroot..."},
      %% Call the changeroot function
      NewEdgeManagement = changeroot(NodeStates, EdgeManagement, LoggingPID),
      mainLoop(NodeStates, NewEdgeManagement, WakeUpTimer, LoggingPID)

  end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%Help Functions%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


recursiveForEachForInitiate([], NodeStates, _, _, _, _) ->
  NodeStates
;

recursiveForEachForInitiate([EdgeRecord| Tail], NodeStates, InboundNodeLevel, InboundFragmentId, InboundState, LoggingPID) ->

  LoggingPID ! {output_send_to_node, "initiate (in recursiveForEach() )", EdgeRecord#edge.node_pid} ,
  EdgeRecord#edge.node_pid ! {initiate, InboundNodeLevel, InboundFragmentId, InboundState, EdgeRecord},

  if
    InboundState == find ->
      NewNodeStates = NodeStates#node{find_count = NodeStates#node.find_count + 1};
  % InboundState != find
    true ->
      NewNodeStates = NodeStates
  end,

  recursiveForEachForInitiate(Tail, NewNodeStates, InboundNodeLevel, InboundFragmentId, InboundState, LoggingPID)
.


wakeUp(NodeStates, EdgeManagement, LoggingPID) ->
  LoggingPID ! {output_functions, "in wakeUp..."},

  MinEdge = utilities:getMinEdge(EdgeManagement#edge_management.edge_list),

  %% HOW TO change edgeRecord state in a list of records!!
  EdgesList = utilities:change_edge_state(EdgeManagement#edge_management.edge_list, MinEdge, branch),
  NewEdgeManagement = EdgeManagement#edge_management{edge_list = EdgesList},

  %% setting node new properties
  NewNodeStates = NodeStates#node{
    state = found,
    level = 0,
    find_count = 0
  },

  % send connect other Node behind MinEdge
  LoggingPID ! {output_send_to_node, "connect (in wakeUp() )", MinEdge#edge.node_pid} ,
  MinEdge#edge.node_pid ! {connect, 0, MinEdge},

  {NewNodeStates, NewEdgeManagement}
.

changeroot(NodeStates, EdgeManagement, LoggingPID) ->
  LoggingPID ! {output_functions, "in changeroot()..."},

  %% Get Best Edge
  BestEdge = EdgeManagement#edge_management.best_edge,

  if
    BestEdge#edge.state == branch ->
      LoggingPID ! {output_send_to_node, "changeroot (in changeroot() )",BestEdge#edge.node_pid} ,
      BestEdge#edge.node_pid ! {changeroot, BestEdge},
      NewEdgeManagement = EdgeManagement;

  %  BestEdgeState == branch
    true ->
      LoggingPID ! {output_send_to_node, "connect (in changeroot() )",BestEdge#edge.node_pid} ,
      BestEdge#edge.node_pid ! {connect, NodeStates#node.level, BestEdge},
      NewBestEdge = BestEdge#edge{state = branch},
      NewEdgeManagement = EdgeManagement#edge_management{best_edge = NewBestEdge}
  end,
  NewEdgeManagement
.

report(NodeStates, EdgeManagement, LoggingPID) ->
  LoggingPID ! {output_functions, "in report() ..."},
  if
    NodeStates#node.find_count == 0 ->
      if
        EdgeManagement#edge_management.test_edge == undefined ->
          %% Set Node State
          NewNodeStates = NodeStates#node{state = found},
          %% Get InBranchEdge
          InBranchEdge = EdgeManagement#edge_management.in_branch_edge,
          LoggingPID ! {output_send_to_node, "report (in report() )",InBranchEdge#edge.node_pid } ,
          %% Send Report Message on InBranchEdge
          InBranchEdge#edge.node_pid ! {report, EdgeManagement#edge_management.best_weight, InBranchEdge};
      % TestEdge != undefined
        true ->
          NewNodeStates = NodeStates
      end;
  % FindCount != 0
    true ->
      NewNodeStates = NodeStates
  end,
  NewNodeStates
.

test(NodeStates, EdgeManagement, LoggingPID) ->
  LoggingPID ! {output_functions, "in test()..."},
  MinBasicEdge = utilities:get_min_basic_edge(EdgeManagement#edge_management.edge_list),

  if
    not MinBasicEdge#edge.weight_id == undefined ->
      %% set Test Edge
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = MinBasicEdge},


      LoggingPID ! {output_send_to_node, "test (in test() )",MinBasicEdge#edge.node_pid  } ,
      %% send Test Message on MinBasicEdge
      MinBasicEdge#edge.node_pid ! {test, NodeStates#node.level, NodeStates#node.fragment_id, MinBasicEdge},
      NewNodeStates = NodeStates;
  %  MinBasicEdge == undefined
    true ->
      %% set Test Edge
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = undefined},
      NewNodeStates = report(NodeStates, NewEdgeManagement, LoggingPID)
  end,
  {NewNodeStates, NewEdgeManagement}
.

haltNow(NodeID, InBranchOfNodeID, LoggingPID) ->
  %   Get PrinterPID for output reasons

  PrinterPID = global:whereis_name("printer"),
  timer:sleep(1000),

  PrinterPID ! {print, NodeID, InBranchOfNodeID} ,

  LoggingPID ! {output_functions, "in haltNow()..."},
  exit(LoggingPID, "LoggingPID shutdown"),
  exit(self(), "knoten_logik shutdown")
.

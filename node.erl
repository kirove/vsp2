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


  LoggingPID ! {pid, ["NodeID", self()]},


  % RandomTime in Seconds
  RandomTimeSeconds = random:uniform(15),

  % Start wakeup-Timer for node
  {_, WakeUpTimer} = timer:send_after(timer:seconds(RandomTimeSeconds), self(), {wake_up_msg}),


  NodeStates = #node{
    name = NodeName,
    state = sleeping,
    find_count = 0,
    fragment_id = undefined,
    level = 0
  },

  EdgeRecordList = utilities:setUpEdgeList(EdgeList, []),


  EdgeManagement = #edge_management{
    edge_list = EdgeRecordList,
    in_branch_edge = undefined,
    best_edge = undefined,
    best_weight = undefined,
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
      LoggingPID ! {output_receive, "in mainLoop -> received wake_up_msg from timer..."},
      if
      % wakeup muss aufgerufen werden wenn knoten noch am schlafen ist
        NodeStates#node.state == sleeping ->
          {FinalNodeStates, FinalEdgeManagement} = wakeUp(NodeStates, EdgeManagement, LoggingPID);


      % Nodestate != sleeping
        true ->
          FinalNodeStates = NodeStates,
          FinalEdgeManagement = EdgeManagement

      end
  ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve CONNECT Message					%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {connect, InboundLevel, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> connect...", InboundEdge#edge.weight_id},

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
          FinalEdgeManagement = NewEdgeManagement#edge_management{edge_list = EdgesList},

          LoggingPID ! {output_send_to_node, "initiate auf (in connect1)", Edge#edge.weight_id},
          %% send Initiate Message
          Edge#edge.node_pid ! {initiate, NewNodeStates#node.level, NewNodeStates#node.fragment_id, NewNodeStates#node.state, Edge},
          if
            NewNodeStates#node.state == find ->
              FinalNodeStates = NewNodeStates#node{find_count = (NewNodeStates#node.find_count + 1)};
            true ->
              FinalNodeStates = NewNodeStates
          end;
      % InboundLevel >= NodeLevel
        true ->

          if
            Edge#edge.state == basic ->
              LoggingPID ! {output_send_to_node, "requeing connect (in connect)", -1},
              self() ! {connect, InboundLevel, InboundEdge};
          % EdgeState != basic
            true ->
              LoggingPID ! {output_send_to_node, "initiate (in connect2)", Edge#edge.weight_id},
              Edge#edge.node_pid ! {initiate, (NewNodeStates#node.level + 1), Edge#edge.weight_id, find, Edge}
          end,
          FinalNodeStates = NewNodeStates,
          FinalEdgeManagement = NewEdgeManagement
      end

  ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve INITIATE Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {initiate, InboundNodeLevel, InboundFragmentId, InboundState, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> initiate...", InboundEdge#edge.weight_id},

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
          {FinalNodeStates, FinalEdgeManagement} = test(NewNodeStatesV2, NewEdgeManagement, LoggingPID);
        true ->
          FinalNodeStates = NewNodeStatesV2,
          FinalEdgeManagement = NewEdgeManagement
      end

  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve TEST Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {test, InboundLevel, InboundFragmentID, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> test...", InboundEdge#edge.weight_id},


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
          LoggingPID ! {output_send_to_node, "requeing test (in test)", -1},
          self() ! {test, InboundLevel, InboundFragmentID, InboundEdge},
          FinalNodeStates = NewNodeStates,
          FinalEdgeManagement = NewEdgeManagement;

      %InboundLevel <= NodeLevel
        true ->
          if
            not (InboundFragmentID == NewNodeStates#node.fragment_id) ->
              LoggingPID ! {output_send_to_node, "accept (in test)", Edge#edge.weight_id},
              Edge#edge.node_pid ! {accept, Edge},
              FinalNodeStates = NewNodeStates,
              FinalEdgeManagement = NewEdgeManagement;
          %  InboundFragmentID == FragmentID
            true ->
              if
                Edge#edge.state == basic ->
                  LoggingPID ! {output_receive, "in mainLoop -> TestMsg setting Edge to Rejected"},
                  EdgesList = utilities:change_edge_state(NewEdgeManagement#edge_management.edge_list, Edge, rejected),
                  NewEdgeManagementV2 = NewEdgeManagement#edge_management{edge_list = EdgesList};
              % EdgeState != basic
                true ->
                  NewEdgeManagementV2 = NewEdgeManagement
              end,
              %% Get TestEdge
              TestEdge = NewEdgeManagementV2#edge_management.test_edge,
              if
                not  (Edge#edge.weight_id == TestEdge#edge.weight_id) ->
                  LoggingPID ! {output_send_to_node, "reject (in test)", Edge#edge.weight_id},
                  Edge#edge.node_pid ! {reject, Edge},
                  FinalNodeStates = NewNodeStates,
                  FinalEdgeManagement = NewEdgeManagementV2;
              % EdgeWeight_ID ==  TestEdgeID
                true ->
                  {FinalNodeStates, FinalEdgeManagement} = test(NewNodeStates, NewEdgeManagementV2, LoggingPID)
              end
          end
      end
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Accept Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {accept, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> accept...", InboundEdge#edge.weight_id},

      %% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
      Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

      %% Set Test Edge undefined
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = undefined},
      %% Get BestEdge


      if
        Edge#edge.weight_id < NewEdgeManagement#edge_management.best_weight ->
          FinalEdgeManagement = NewEdgeManagement#edge_management{
            best_edge = Edge,
            best_weight = Edge#edge.weight_id
          };
      % EdgeWeight_ID >= BestWeight
        true ->
          FinalEdgeManagement = NewEdgeManagement
      end,
      %% Call Report Function
      FinalNodeStates = report(NodeStates, FinalEdgeManagement, LoggingPID)
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Reject Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {reject, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> reject...", InboundEdge#edge.weight_id},

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
      {FinalNodeStates, FinalEdgeManagement} = test(NodeStates, NewEdgeManagement, LoggingPID)
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%				Recieve Report Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {report, InboundWeight, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> report...", InboundEdge#edge.weight_id},

      %% Get the EdgeRecord from our Record for later use, since the NodePID and the state are different
      Edge = utilities:get_edge(EdgeManagement#edge_management.edge_list, InboundEdge),

      %% Get InBranchEdge
      InBranchEdge = EdgeManagement#edge_management.in_branch_edge,
      EqualEdges = utilities:proveEqual(InBranchEdge, Edge),
      if
        not EqualEdges ->
          NewNodeStates = NodeStates#node{find_count = (NodeStates#node.find_count - 1)},

          if
            InboundWeight < EdgeManagement#edge_management.best_weight ->
              FinalEdgeManagement = EdgeManagement#edge_management{
                best_edge = Edge,
                best_weight = InboundWeight
              };
          %InboundWeight >= BestWeight
            true ->
              FinalEdgeManagement = EdgeManagement
          end,
          FinalNodeStates = report(NewNodeStates, FinalEdgeManagement, LoggingPID);

      % InBranchEdge == Edge
        true ->
          if
            NodeStates#node.state == find ->
              LoggingPID ! {output_send_to_node, "requeueing report (in report)", -1},
              self() ! {report, InboundWeight, InboundEdge},
              FinalEdgeManagement = EdgeManagement;
          %NodeState != find
            true ->
              if
                (InboundWeight > EdgeManagement#edge_management.best_weight) ->
                  FinalEdgeManagement = changeroot(NodeStates, EdgeManagement, LoggingPID);

              %InboundWeight <= BestWeight
                true ->

                  if
                    (InboundWeight == infinity) and (EdgeManagement#edge_management.best_weight == infinity) ->

                      FinalEdgeManagement = EdgeManagement,

                      EdgeManagement#edge_management.in_branch_edge#edge.node_pid ! {halt},

                      haltNow(NodeStates#node.name, EdgeManagement, LoggingPID);


                  %% IsInboundWeightInfinite or BestWeight != infinity
                    true ->
                      FinalEdgeManagement = EdgeManagement
                  end

              end
          end,
          FinalNodeStates = NodeStates
      end
  ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%			Recieve ChangeRoot Message				%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {changeroot, InboundEdge} ->
      LoggingPID ! {received_msg_on_edge, "in mainLoop -> changeroot...", InboundEdge#edge.weight_id},
      %% Call the changeroot function
      FinalEdgeManagement = changeroot(NodeStates, EdgeManagement, LoggingPID),
      FinalNodeStates = NodeStates;

    {halt} ->
      LoggingPID ! {output_receive, "in mainLoop -> halt..."},
      haltNow(NodeStates#node.name, EdgeManagement, LoggingPID),
      FinalEdgeManagement = EdgeManagement,
      FinalNodeStates = NodeStates
  end,

  mainLoop(FinalNodeStates, FinalEdgeManagement, WakeUpTimer, LoggingPID)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%Help Functions%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


recursiveForEachForInitiate([], NodeStates, _, _, _, _) ->
  NodeStates
;

recursiveForEachForInitiate([EdgeRecord| Tail], NodeStates, InboundNodeLevel, InboundFragmentId, InboundState, LoggingPID) ->

  LoggingPID ! {output_send_to_node, "initiate (in recursiveForEach() )", EdgeRecord#edge.weight_id},
  EdgeRecord#edge.node_pid ! {initiate, InboundNodeLevel, InboundFragmentId, InboundState, EdgeRecord},

  if
    InboundState == find ->
      NewNodeStates = NodeStates#node{find_count = (NodeStates#node.find_count + 1)};
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
  LoggingPID ! {output_send_to_node, "connect (in wakeUp() )", MinEdge#edge.weight_id},
  MinEdge#edge.node_pid ! {connect, 0, MinEdge},

  {NewNodeStates, NewEdgeManagement}
.

changeroot(NodeStates, EdgeManagement, LoggingPID) ->
  LoggingPID ! {output_functions, "in changeroot()..."},

  %% Get Best Edge
  BestEdge = EdgeManagement#edge_management.best_edge,

  if
    BestEdge#edge.state == branch ->
      LoggingPID ! {output_send_to_node, "changeroot (in changeroot() )", BestEdge#edge.weight_id},
      BestEdge#edge.node_pid ! {changeroot, BestEdge},
      NewEdgeManagement = EdgeManagement;

  %  BestEdgeState == branch
    true ->
      LoggingPID ! {output_send_to_node, "connect (in changeroot() )", BestEdge#edge.weight_id},
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
      LoggingPID ! {output_functions, "in report() / find_count == 0  ..."},

%%
%%       %%%% nur zur ausgabe
%%       if
%%         is_atom(EdgeManagement#edge_management.test_edge) ->
%%           LoggingPID ! {output_atom, "test_edge: ", EdgeManagement#edge_management.test_edge};
%%         true ->
%%           LoggingPID ! {output_2_string_args, "test_edge: ", EdgeManagement#edge_management.test_edge#edge.weight_id}
%%       end,
%%       %%%%


      if
        EdgeManagement#edge_management.test_edge == undefined ->
          LoggingPID ! {output_functions, "in report() / if / test_edge == undefined ..."},
          %% Set Node State
          NewNodeStates = NodeStates#node{state = found},
          %% Get InBranchEdge
          InBranchEdge = EdgeManagement#edge_management.in_branch_edge,
          LoggingPID ! {output_send_to_node, "report (in report() )", InBranchEdge#edge.weight_id},
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
  IsMinBasicEdgeUndefined = MinBasicEdge#edge.weight_id == undefined,
  if

  %  MinBasicEdge == undefined
    IsMinBasicEdgeUndefined ->
      %% set Test Edge
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = undefined},
      NewNodeStates = report(NodeStates, NewEdgeManagement, LoggingPID);
  %  MinBasicEdge != undefined
    true ->
      %% set Test Edge
      NewEdgeManagement = EdgeManagement#edge_management{test_edge = MinBasicEdge},


      LoggingPID ! {output_send_to_node, "test (in test() )", MinBasicEdge#edge.weight_id},
      %% send Test Message on MinBasicEdge
      MinBasicEdge#edge.node_pid ! {test, NodeStates#node.level, NodeStates#node.fragment_id, MinBasicEdge},
      NewNodeStates = NodeStates
  end,
  {NewNodeStates, NewEdgeManagement}
.

haltNow(NodeID, EdgeManagement, LoggingPID) ->

  InBranchOfNodeID = EdgeManagement#edge_management.in_branch_edge#edge.weight_id,

  OutBranchesList = utilities:get_branch_edges_without(EdgeManagement#edge_management.edge_list, InBranchOfNodeID),

  foreachForHaltNow(OutBranchesList, LoggingPID),
  %lists:foreach(fun(EdgeRecord) -> EdgeRecord#edge.node_pid ! {halt}, LoggingPID ! {output_send_to_node, "halt (in haltnow() )", EdgeRecord#edge.weight_id} end, OutBranchesList),

  %   Get PrinterPID for output reasons
  PrinterPID = global:whereis_name("printer"),
  timer:sleep(1000),


  PrinterPID ! {print, NodeID, InBranchOfNodeID},

  LoggingPID ! {output_functions, "in haltNow()..."},
  exit(LoggingPID, "LoggingPID shutdown"),
  exit(self(), "knoten_logik shutdown")
.


foreachForHaltNow([], _) ->
  ok
;


foreachForHaltNow([{EdgeRecord} | Tail], LoggingPID) ->
  EdgeRecord#edge.node_pid ! {halt},
  LoggingPID ! {output_send_to_node, "halt (in haltnow() )", EdgeRecord#edge.weight_id},
  foreachForHaltNow(Tail, LoggingPID)
.

%%%-------------------------------------------------------------------
%%% @author Akcicek, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2013 1:33 PM
%%%-------------------------------------------------------------------
-module(knoten).
-author("Akcicek, Berngruber").

%% API
-export([start/0, wakeUp/3]).
-import(werkzeug, [get_config_value/2, logging/2]).
-import(knoten_zustand, [start_knoten_zustand/1]).
-import(kanten_verwaltung, [start_kanten_verwaltung/1]).
-import(logging, [start/1]).



start() ->

  % Read out the config file.
  {ok, ConfigListe} = file:consult("node.cfg"),
  {ok, NodeName} = get_config_value(nodename, ConfigListe),
  {ok, StartUpTime} = get_config_value(startuptime, ConfigListe),

  io:format("nodename: ~s~n", [NodeName]),
  % LogFile
  {ok, ServerHostname} = inet:gethostname(),
  % creates filename for the flogging file (append, like string-append)
  ServerLogFileName = lists:append(["Node", ServerHostname, ".log"]),
  % start logging module
  LoggingPID = spawn(fun() -> logging:start(ServerLogFileName) end),
  LoggingPID ! {start},

  % Register the node.
  % node gets registered as the alias
  global:register_name(NodeName, self()),

  % sleep, take time to start the other nodes
  timer:sleep(StartUpTime),
  % start knoten_zustand process
  Knoten_zustandPID = spawn(fun() -> start_knoten_zustand(LoggingPID) end),

  % start kanten_verwaltung process
  Kanten_verwaltungPID = spawn(fun() -> start_kanten_verwaltung(LoggingPID) end),


  LoggingPID ! {pid, ["Knoten", self()]},
  LoggingPID ! {pid, ["Logging", LoggingPID]},
  LoggingPID ! {pid, ["Kanten Verwaltung", Kanten_verwaltungPID]},
  LoggingPID ! {pid, ["Knoten Zustand", Knoten_zustandPID]},


  % RandomTime in Seconds
  RandomTimeSeconds = random:uniform(10),

  % Start wakeup-Timer for node
  {_, WakeUpTimer} = timer:apply_after(timer:seconds(RandomTimeSeconds), knoten, wakeUp, [Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID]),


  % start mainLoop
  waitingRoom(WakeUpTimer, Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
.

%node sits in waitingRoom after node initiation until received a message or WakeUpTimer timed out
waitingRoom(WakeUpTimer, Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  LoggingPID ! {output_knoten, "in WatingRoom..."},
  receive
    {connect, InboundLevel, Edge} ->
      LoggingPID ! {output_receive, "in WatingRoom -> connect..."},
      timer:cancel(WakeUpTimer),
      wakeUp(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID),
      self() ! {connect, InboundLevel, Edge},
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);

    {test, Level, FragmentID, Edge} ->
      LoggingPID ! {output_receive, "in WatingRoom -> test..."},
      timer:cancel(WakeUpTimer),
      wakeUp(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID),
      self() ! {test, Level, FragmentID, Edge},
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
  end
.


mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  receive
    {connect, InboundLevel, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> connect..."},
      Knoten_zustandPID ! {get_level, self()},
      receive
        {node_level, NodeLevel} ->
          ok
      end,

      if
        InboundLevel < NodeLevel ->
          Kanten_verwaltungPID ! {set_edge_to, branch, Edge},
          Knoten_zustandPID ! {get_fragment_id, self()},
          receive
            {fragment_id, FragmentID} ->
              LoggingPID ! {output_receive, "in mainLoop -> connect/fragment_id..."},
              Knoten_zustandPID ! {get_node_state, self()},
              receive
                {node_state, NodeState} ->
                  LoggingPID ! {output_receive, "in mainLoop -> connect/fragment_id/node_state..."},
                  {EdgeWeight_ID, _} = Edge,
                  Kanten_verwaltungPID ! {get_edge, EdgeWeight_ID, self()},
                  receive
                    {edge, WantedEdge} ->
                      LoggingPID ! {output_receive, "in mainLoop -> connect/fragment_id/node_state/edge..."},
                      {_, WantedNodePID} = WantedEdge,
                      WantedNodePID ! {initiate, NodeLevel, FragmentID, NodeState, WantedEdge},
                      if
                        NodeState == find ->
                          Knoten_zustandPID ! {find_count_plus_one};
                        true ->
                          ok
                      end
                  end
              end

          end;
      % InboundLevel >= NodeLevel
        true ->
          Kanten_verwaltungPID ! {get_edge_state, Edge, self()},
          receive
            {edge_state, EdgeState} ->
              LoggingPID ! {output_receive, "in mainLoop -> connect/edge_state..."},
              if
                EdgeState == basic ->
                  self() ! {connect, InboundLevel, Edge};
              % EdgeState != basic
                true ->
                  Knoten_zustandPID ! {get_level, self()},
                  receive
                    {node_level, Level} ->
                      LoggingPID ! {output_receive, "in mainLoop -> connect/edge_state/node_level..."},
                      {EdgeID, _} = Edge,
                      Kanten_verwaltungPID ! {get_edge, EdgeID, self()},
                      receive
                        {edge, WantedEdge} ->
                          LoggingPID ! {output_receive, "in mainLoop -> connect/edge_state/node_level/edge..."},
                          {WantedEdgeWeight_ID, WantedNodePID} = WantedEdge,

                          WantedNodePID ! {initiate, Level + 1, WantedEdgeWeight_ID, find, WantedEdge}
                      end
                  end
              end
          end
      end,
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);


    {initiate, InboundNodeLevel, InboundFragmentId, InboundState, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> initiate..."},
      Knoten_zustandPID ! {set_level, InboundNodeLevel},
      Knoten_zustandPID ! {set_fragment_id, InboundFragmentId},
      Knoten_zustandPID ! {set_node_state, InboundState},
      Kanten_verwaltungPID ! {set_in_branch, Edge},
      Kanten_verwaltungPID ! {set_best_edge, undefined},
      Kanten_verwaltungPID ! {set_best_weight, infinity},

      Kanten_verwaltungPID ! {get_branch_edges_without, Edge, self()},
      receive
        {branch_list_without_unwanted_edge, BranchListWithoutUnwantedEdge} ->
          LoggingPID ! {output_receive, "in mainLoop -> initiate/branch_list_without_unwanted_edge..."},
          lists:foreach(fun({EdgeWeight_ID, NodePID}) ->
            NodePID ! {initiate, InboundNodeLevel, InboundFragmentId, InboundState, {EdgeWeight_ID, NodePID}},
            if
              InboundState == find ->
                Knoten_zustandPID ! {find_count_plus_one};
              true ->
                ok
            end
          end,
            BranchListWithoutUnwantedEdge),

          if
            InboundState == find ->
              test(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
          end
      end,
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);


    {test, InboundLevel, InboundFragmentID, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> test..."},
      Knoten_zustandPID ! {get_node_state, self()},
      receive
        {node_state, NodeState} ->
          LoggingPID ! {output_receive, "in mainLoop -> test/node_state..."},
          if
            NodeState == sleeping ->
              wakeUp(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);

          % NodeState != sleeping
            true ->
              Knoten_zustandPID ! {get_level, self()},
              receive
                {node_level, NodeLevel} ->
                  LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level..."},
                  if
                    InboundLevel > NodeLevel ->
                      self() ! {test, InboundLevel, InboundFragmentID, Edge};

                  %InboundLevel <= NodeLevel
                    true ->
                      Knoten_zustandPID ! {get_fragment_id, self()},
                      receive
                        {fragment_id, FragmentID} ->
                          LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level/fragment_id..."},
                          if
                            not InboundFragmentID == FragmentID ->
                              {EdgeWeight_ID, _} = Edge,
                              Kanten_verwaltungPID ! {get_edge, EdgeWeight_ID, self()},
                              receive
                                {edge, WantedEdge} ->
                                  LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level/fragment_id/edge..."},
                                  {_, WantedNodePID} = WantedEdge,
                                  WantedNodePID ! {accept, Edge}
                              end;

                          %  InboundFragmentID == FragmentID
                            true ->
                              Kanten_verwaltungPID ! {get_edge_state, Edge, self()},
                              receive
                                {edge_state, EdgeState} ->
                                  LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level/fragment_id/edge_state..."},
                                  if
                                    EdgeState == basic ->
                                      Kanten_verwaltungPID ! {set_edge_to, rejected, Edge};

                                  % EdgeState != basic
                                    true ->
                                      ok

                                  end,
                                  Kanten_verwaltungPID ! {get_test_edge, self()},
                                  receive
                                    {test_edge, TestEdge} ->
                                      LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level/fragment_id/edge_state/test_edge..."},
                                      {EdgeWeight_ID, _} = Edge,
                                      Kanten_verwaltungPID ! {get_edge, EdgeWeight_ID, self()},
                                      receive
                                        {edge, WantedEdge} ->
                                          LoggingPID ! {output_receive, "in mainLoop -> test/node_state/node_level/fragment_id/edge_state/test_edge/edge..."},
                                          {_, WantedNodePID} = WantedEdge,
                                          {TestEdgeID, _} = TestEdge,
                                          if
                                            not  EdgeWeight_ID == TestEdgeID ->
                                              WantedNodePID ! {reject, Edge};

                                          % EdgeWeight_ID ==  TestEdgeID
                                            true ->
                                              test(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
                                          end
                                      end
                                  end
                              end
                          end
                      end

                  end

              end

          end
      end,


      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);


    {accept, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> accept..."},
      Kanten_verwaltungPID ! {set_test_edge, undefined},
      Kanten_verwaltungPID ! {get_best_weight, self()},
      {EdgeWeight_ID, _} = Edge,

      receive
        {best_weight, BestWeight} ->
          LoggingPID ! {output_receive, "in mainLoop -> accept/best_weight..."},
          if
            EdgeWeight_ID < BestWeight ->
              Kanten_verwaltungPID ! {set_best_edge, Edge},
              Kanten_verwaltungPID ! {set_best_weight, EdgeWeight_ID};

          % EdgeWeight_ID >= BestWeight
            true ->
              ok

          end,
          report(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
      end,


      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);


    {reject, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> reject/..."},
      Kanten_verwaltungPID ! {get_edge_state, Edge, self()},
      receive
        {edge_state, EdgeState} ->
          LoggingPID ! {output_receive, "in mainLoop -> reject/edge_state..."},
          if
            EdgeState == basic ->
              Kanten_verwaltungPID ! {set_edge_to, rejected, Edge};
            true ->
              ok
          end,
          test(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
      end,
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);

    {report, InboundWeight, Edge} ->
      LoggingPID ! {output_receive, "in mainLoop -> report..."},
      Kanten_verwaltungPID ! {get_in_branch, self()},
      receive
        {in_branch, InBranchEdge} ->
          LoggingPID ! {output_receive, "in mainLoop -> report/in_branch..."},
          if
            not InBranchEdge == Edge ->
              Knoten_zustandPID ! {find_count_minus_one},
              Kanten_verwaltungPID ! {get_best_weight, self()},
              receive
                {best_weight, BestWeight} ->
                  LoggingPID ! {output_receive, "in mainLoop -> report/in_branch/best_weight..."},
                  if
                    InboundWeight < BestWeight ->
                      Kanten_verwaltungPID ! {set_best_weight, InboundWeight},
                      Kanten_verwaltungPID ! {set_best_edge, Edge};

                  %InboundWeight >= BestWeight
                    true ->
                      ok


                  end,
                  report(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
              end;

          %  InBranchEdge == Edge
            true ->
              Knoten_zustandPID ! {get_node_state, self()},
              receive
                {node_state, NodeState} ->
                  LoggingPID ! {output_receive, "in mainLoop -> report/in_branch/node_state..."},
                  if
                    NodeState == find ->
                      self() ! {report, InboundWeight, Edge};

                  %NodeState != find
                    true ->
                      Kanten_verwaltungPID ! {get_best_weight, self()},
                      receive
                        {best_weight, BestWeight} ->
                          LoggingPID ! {output_receive, "in mainLoop -> report/in_branch/node_state/best_weight..."},
                          if
                            InboundWeight > BestWeight ->
                              changeroot(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);

                          %InboundWeight <= BestWeight
                            true ->
                              IsInboundWeightInfinite = InboundWeight == infinity,
                              if

                                IsInboundWeightInfinite and BestWeight == infinity ->

                                  haltNow(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);
                                true ->
                                  ok

                              end
                          end
                      end

                  end

              end


          end
      end,
      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID);

    {changeroot, _} ->
      LoggingPID ! {output_receive, "in mainLoop -> changeroot..."},
      changeroot(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID),

      mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)


  end

.


haltNow(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  LoggingPID ! {output_functions, "in haltNow()..."},
  exit(Knoten_zustandPID, "knoten_zustand shutdown"),
  exit(Kanten_verwaltungPID, "kanten_verwaltung shutdown"),
  exit(self(), "knoten_logik shutdown")


.



changeroot(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  LoggingPID ! {output_functions, "in changeroot()..."},
  Kanten_verwaltungPID ! {get_best_edge, self()},
  receive
    {best_edge, BestEdge} ->
      LoggingPID ! {output_functions, "in changeroot() -> best_edge..."},
      Kanten_verwaltungPID ! {get_edge_state, BestEdge, self()},
      receive
        {edge_state, BestEdgeState} ->
          LoggingPID ! {output_functions, "in changeroot() -> best_edge/edge_state..."},
          if
            BestEdgeState == branch ->
              {_, NodePID} = BestEdge,
              NodePID ! {changeroot, BestEdge};

          %  BestEdgeState == branch
            true ->
              Knoten_zustandPID ! {get_level, self()},
              receive
                {node_level, NodeLevel} ->
                  LoggingPID ! {output_functions, "in changeroot() -> best_edge/edge_state/node_level..."},
                  {_, NodePID} = BestEdge,
                  NodePID ! {connect, NodeLevel, BestEdge},
                  Kanten_verwaltungPID ! {set_best_edge, branch}

              end


          end


      end


  end
.


report(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  LoggingPID ! {output_functions, "in report() ..."},
  Knoten_zustandPID ! {get_find_count, self()},
  receive
    {find_count, FindCount} ->
      LoggingPID ! {output_functions, "in report() -> find_count..."},
      if
        FindCount == 0 ->
          Kanten_verwaltungPID ! {get_test_edge, self()},
          receive
            {test_edge, TestEdge} ->
              LoggingPID ! {output_functions, "in report() -> find_count/test_edge..."},
              if
                TestEdge == undefined ->
                  Knoten_zustandPID ! {set_node_state, found},
                  Kanten_verwaltungPID ! {get_in_branch, self()},
                  receive
                    {in_branch, InBranchEdge} ->
                      LoggingPID ! {output_functions, "in report() -> find_count/test_edge/in_branch..."},
                      Kanten_verwaltungPID ! {get_best_weight, self()},
                      receive
                        {best_weight, BestWeight} ->
                          LoggingPID ! {output_functions, "in report() -> find_count/test_edge/in_branch/best_weight..."},
                          {_, NodePID} = InBranchEdge,
                          NodePID ! {report, BestWeight, InBranchEdge}

                      end
                  end;

              % TestEdge != undefined
                true ->
                  ok

              end
          end;

      % FindCount != 0
        true ->
          ok
      end
  end
.


test(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->
  LoggingPID ! {output_functions, "in test()..."},
  Kanten_verwaltungPID ! {get_min_basic_edge, self()},
  receive
    {min_basic_edge, MinBasicEdge} ->
      LoggingPID ! {output_functions, "in test() -> min_basic_edge..."},
      {EdgeWeight_ID, NodePID} = MinBasicEdge,
      if
        not EdgeWeight_ID == undefined ->
          Kanten_verwaltungPID ! {set_test_edge, MinBasicEdge},
          Knoten_zustandPID ! {get_level, self()},
          receive
            {node_level, NodeLevel} ->
              LoggingPID ! {output_functions, "in test() -> min_basic_edge/node_level..."},
              Knoten_zustandPID ! {get_fragment_id, self()},
              receive
                {fragment_id, FragmentID} ->
                  LoggingPID ! {output_functions, "in test() -> min_basic_edge/node_level/fragment_id..."},
                  {_, NodePID} = MinBasicEdge,
                  NodePID ! {test, NodeLevel, FragmentID, MinBasicEdge}
              end
          end;
      %  MinBasicEdge == undefined
        true ->
          Kanten_verwaltungPID ! {set_test_edge, undefined},
          report(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
      end

  end
.


wakeUp(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID) ->

  LoggingPID ! {output_functions, "in wakeUp..."},

  Kanten_verwaltungPID ! {get_min_edge, self()},


  receive
    {min_edge, {EdgeWeight_ID, NodePID}} ->

      LoggingPID ! {output_functions, "in wakeUp -> min_edge..."},
      Kanten_verwaltungPID ! {set_edge_to, branch, {EdgeWeight_ID, NodePID}}
  end,

  Knoten_zustandPID ! {set_level, 0},
  Knoten_zustandPID ! {set_node_state, found},
  Knoten_zustandPID ! {set_find_count, 0},


  % send conncect other Node behind MinEdge
  NodePID ! {connect, 0, {EdgeWeight_ID, NodePID}},
  LoggingPID ! {send_connect_to, NodePID},
  mainLoop(Knoten_zustandPID, Kanten_verwaltungPID, LoggingPID)
.
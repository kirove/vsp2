%%%-------------------------------------------------------------------
%%% @author Akcicek, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2013 1:34 PM
%%%-------------------------------------------------------------------
-module(kanten_verwaltung).
-author("Akcicek, Berngruber").

%% API
-export([start_kanten_verwaltung/1]).
-import(werkzeug, [get_config_value/2, logging/2]).




start_kanten_verwaltung(LoggingPID) ->




  % start mainLoop
  mainLoop(NewEdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID).




mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID) ->
  LoggingPID ! {output_kanten_verwaltung, "Kantenverwaltung: in mainLoop..."},

  receive
    {get_min_edge, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_min_edge..."},

      MinEdge = getMinEdge(EdgeList),
      NodeLogicPID ! {min_edge, MinEdge},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_test_edge, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_test_edge..."},
      NodeLogicPID ! {test_edge, TestEdge},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_edge_state, {EdgeID, _}, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_edge_state..."},
      EdgeState = getEdgeState(EdgeID, EdgeList),
      LoggingPID ! {output_kanten_verwaltung_atom, "in mainLoop -> get_edge_state, EdgeStae is", EdgeState},
      NodeLogicPID ! {edge_state, EdgeState},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_branch_edges_without, {EdgeID, _}, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_branch_edges_without..."},
      BranchEdgeList = getEdgesWithState(branch, EdgeList),
      BranchListWithoutUnwantedEdge = lists:keydelete(EdgeID, 1, BranchEdgeList),
      NodeLogicPID ! {branch_list_without_unwanted_edge, BranchListWithoutUnwantedEdge},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_min_basic_edge, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_min_basic_edge..."},
      BasicEdgeList = getEdgesWithState(basic, EdgeList),
      MinBasicEdge = getMinEdge(BasicEdgeList),
      NodeLogicPID ! {min_basic_edge, MinBasicEdge},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_best_weight, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_best_weight..."},
      NodeLogicPID ! {best_weight, BestWeight},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_in_branch, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_in_branch..."},
      NodeLogicPID ! {in_branch, InBranchEdge},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {get_edge, EdgeID, NodeLogicPID} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> get_edge..."},
      {EdgeId, NodePID, _} = lists:keyfind(EdgeID, 1, EdgeList),
      NodeLogicPID ! {edge, {EdgeId, NodePID}},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);




    {set_edge_to, State, Edge} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> set_edge_to..."},
      NewEdgeList = setEdgeState(Edge, State, EdgeList),
      mainLoop(NewEdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {set_in_branch, Edge} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> set_in_branch..."},
      mainLoop(EdgeList, Edge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {set_best_edge, BestEdge} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> set_best_edge..."},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {set_best_weight, BestWeight} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> set_best_weight..."},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, TestEdge, LoggingPID);

    {set_test_edge, NewTestEdge} ->
      LoggingPID ! {output_kanten_verwaltung, "in mainLoop -> set_test_edge..."},
      mainLoop(EdgeList, InBranchEdge, BestEdge, BestWeight, NewTestEdge, LoggingPID)


  end
.


getEdgesWithState(SearchState, EdgeList) ->
  {Satisfying, _} = lists:partition(fun({_, _, EdgeState}) -> EdgeState == SearchState end, EdgeList),
  Satisfying
.


getEdgeState(EdgeID, EdgeList) ->
  {_, _, EdgeState} = lists:keyfind(EdgeID, 1, EdgeList),
  EdgeState
.


% update the State of edge with EdgeID
setEdgeState({EdgeID, NodePID}, NewState, EdgeList) ->
  lists:keyreplace(EdgeID, 3, EdgeList, {EdgeID, NodePID, NewState})
.




getMinEdge([]) ->
  {undefined, undefined}
;

% startcall with only one list as an argument
getMinEdge([{EdgeId, NodePID, State}| Tail]) ->
  getMinEdge(Tail, {EdgeId, NodePID, State}).

getMinEdge([], MinElem) ->
  {EdgeId, NodePID, _} = MinElem,
  {EdgeId, NodePID};

% main recursion
getMinEdge([{EdgeId, NodePID, State}| Tail], {LowestEdgeId, NodePIDofLowestEdge, StateOfLowestEdge}) ->
  if
    EdgeId < LowestEdgeId ->
      getMinEdge(Tail, {EdgeId, NodePID, State});
  %EdgeId >=  LowestEdgeId: no smaller id found, no change needed,
    true ->
      getMinEdge(Tail, {LowestEdgeId, NodePIDofLowestEdge, StateOfLowestEdge})
  end
.





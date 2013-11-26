%%%-------------------------------------------------------------------
%%% @author Akcicek, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2013 1:34 PM
%%%-------------------------------------------------------------------
-module(utilities).
-author("Akcicek, Berngruber, Osman").

%% API
-compile(export_all).

-include_lib("Datastructure.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Getting the minEdge out of a list of records
getMinEdge([]) ->
  #edge{
    weight_id = undefined,
    node_pid = undefined,
    state = undefined
  };

% startcall with only one list as an argument
getMinEdge([EdgeListRecord| Tail]) ->
  getMinEdge(Tail, EdgeListRecord).

getMinEdge([], LowestWeightEdgeRecord) ->
  LowestWeightEdgeRecord;

% main recursion
getMinEdge([EdgeRecord| Tail], LowestWeightEdgeRecord) ->
  if
    EdgeRecord#edge.weight_id < LowestWeightEdgeRecord#edge.weight_id ->
      getMinEdge(Tail, EdgeRecord);
  %#edge.weight_id >=  #LowestEdge.weight_id: no smaller id found, no change needed,
    true ->
      getMinEdge(Tail, LowestWeightEdgeRecord)
  end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% setting a state of a given Edge and raising an Error if edge not found

change_edge_state(EdgeRecordList, EdgeRecordToChange, State) ->
  change_edge_state(EdgeRecordList, [], EdgeRecordToChange, State)
.
change_edge_state([], _, _, _) ->
  erlang:error("Error in utilities:change_edge_state, no such element in list")
;
change_edge_state([EdgeRecord| Tail], UpdatedEdgeList, EdgeRecordToChange, State) ->
  if
    (EdgeRecord#edge.weight_id == EdgeRecordToChange#edge.weight_id) ->
      %found elem, updating...
      NewEdgeRecord = EdgeRecord#edge{state = State},
      [Tail | [NewEdgeRecord | UpdatedEdgeList]];
  % didn't find elem
    true ->
      change_edge_state(Tail, [EdgeRecord | UpdatedEdgeList], EdgeRecordToChange, State)
  end

.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% gets the PID for every service
setUpEdgeList([], EdgeListRecord) ->
  EdgeListRecord;

setUpEdgeList([{EdgeName, ServiceName} | Tail], EdgeRecordList) ->

  NodePID = global:whereis_name(ServiceName),
  timer:sleep(1000),

  Edge = #edge{
    weight_id = EdgeName,
    node_pid = NodePID,
    state = basic
  },

  NewEdgeRecordList = [Edge | EdgeRecordList],
  setUpEdgeList(Tail, NewEdgeRecordList)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get edgeRecord from EdgeList
get_edge([], _) ->
  edge_not_found
;
get_edge([EdgeRecord| Tail], EdgeRecordToFind) ->
  if
    EdgeRecord#edge.weight_id == EdgeRecordToFind#edge.weight_id ->
      EdgeRecord;
  %   EdgeRecord#edge.weight_id != EdgeRecordToFind#edge.weight_id
    true ->
      get_edge(Tail, EdgeRecordToFind)
  end
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get all branch Edges excluding the given Edge

%% add 1 Parameter to the Head of the Procedure
get_branch_edges_without(EdgeRecordList, ExcludedEdgeRecord) ->
  get_branch_edges_without(EdgeRecordList, ExcludedEdgeRecord, [])
.

get_branch_edges_without([], _, ResultList) ->
  ResultList
;
get_branch_edges_without([EdgeRecord| Tail], ExcludedEdgeRecord, ResultList) ->
  if
    EdgeRecord#edge.state == branch ->
      if not
      (EdgeRecord#edge.weight_id == ExcludedEdgeRecord#edge.weight_id) ->
        NewResultList = [EdgeRecord | ResultList],
        get_branch_edges_without(Tail, ExcludedEdgeRecord, NewResultList);

      %% EdgeRecord#edge.weight_id == ExcludedEdgeRecord#edge.weight_id
        true ->
          get_branch_edges_without(Tail, ExcludedEdgeRecord, ResultList)
      end;
  %% EdgeState != branch
    true ->
      get_branch_edges_without(Tail, ExcludedEdgeRecord, ResultList)
  end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get all Basic Edges 
get_all_basic_edges([], ResultList) ->
  ResultList
;
get_all_basic_edges([EdgeRecord| Tail], ResultList) ->
  if
    EdgeRecord#edge.state == basic ->
      NewResultList = [EdgeRecord | ResultList],
      get_all_basic_edges(Tail, NewResultList);
  %% Edge != basic
    true ->
      get_all_basic_edges(Tail, ResultList)
  end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get the Min Basic Edge 
get_min_basic_edge(EdgesRecordList) ->

  AllBasicEdges = get_all_basic_edges(EdgesRecordList, []),
  MinEdge = getMinEdge(AllBasicEdges),

  %% Return MinEdge
  MinEdge
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check if two Edges are Equal
%% Edges are Equal when they have the same Weight and the same NodePID ..NOT State			
proveEqual(Edge1, Edge2) ->
  if
    (Edge1 == undefined) or (Edge2 == undefined) ->
      false;
    true ->
      (Edge1#edge.weight_id == Edge2#edge.weight_id)
        and (Edge1#edge.node_pid == Edge2#edge.node_pid)
  end
.		
			
			
			
			
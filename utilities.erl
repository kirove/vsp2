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
  {undefined, undefined};
  
% startcall with only one list as an argument
getMinEdge([EdgeListRecord| Tail]) ->
  getMinEdge(Tail, EdgeListRecord).

getMinEdge([], LowestEdgeListRecord) ->
  LowestEdgeListRecord;

% main recursion
getMinEdge([EdgeRecord| Tail], LowestEdgeRecord) ->
  if
    EdgeRecord#edge.weight_id < LowestEdgeRecord#edge.weight_id ->
      getMinEdge(Tail, EdgeRecord);
  %#edge.weight_id >=  #LowestEdge.weight_id: no smaller id found, no change needed,
    true ->
      getMinEdge(Tail, LowestEdgeRecord)
  end
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% setting a state of a given Edge and raising an Error if edge not found

change_edge_state(EdgeRecordList, EdgeRecordToChange, State) ->
  change_edge_state(EdgeRecordList, [], EdgeRecordToChange, State)
.
change_edge_state([],_,  _, _) ->
  erlang:error("Error in utilities:change_edge_state, no such element in list")
;
change_edge_state([EdgeRecord| Tail], UpdatedEdgeList, EdgeRecordToChange, State) ->
	if
		(EdgeRecord#edge.weight_id == EdgeRecordToChange#edge.weight_id) ->
		  %found elem, updating...
				NewEdgeRecord = EdgeRecord#edge{ state = State },
    [ Tail | [NewEdgeRecord | UpdatedEdgeList ]];
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

setUpEdgeList([{EdgeName, ServiceName} | Tail], EdgeListRecord) ->

  NodePID = global:whereis_name(ServiceName),
  timer:sleep(1000),

  Edge = #edge {
    weight_id = EdgeName,
    node_pid = NodePID,
    state = basic
  },

  NewEdgeListRecord = [Edge | EdgeListRecord],
  setUpEdgeList(Tail, NewEdgeListRecord)
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get edgeRecord from EdgeList
get_edge([], _) ->
	edge_not_found
;
get_edge([EdgeListRecord| Tail], RecordToFind) ->
	if
		EdgeListRecord#edge.weight_id == RecordToFind#edge.weight_id ->
			EdgeListRecord;
	true ->
		get_edge(Tail, RecordToFind)
	end
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get all branch Edges excluding the given Edge

%% add 1 Parameter to the Head of the Procedure
get_branch_edges_without(EdgesList, ExcludedEdge) ->
	get_branch_edges_without(EdgesList, ExcludedEdge, [])
.

get_branch_edges_without([], _, ResultList) ->
	ResultList
;
get_branch_edges_without([EdgeListRecord| Tail], ExcludedEdge, ResultList) ->
	if
		EdgeListRecord#edge.state == branch ->
			if not
				EdgeListRecord#edge.weight_id == ExcludedEdge#edge.weight_id ->
						NewResultList = [ EdgeListRecord | ResultList],
						get_branch_edges_without(Tail, ExcludedEdge, NewResultList);
				true->
					get_branch_edges_without(Tail, ExcludedEdge, ResultList)
			end;
		%% EdgeState != branch	
		true ->
			get_branch_edges_without(Tail, ExcludedEdge, ResultList)
	end		
.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get all Basic Edges 
get_all_basic_edges([], ResultList) ->
	ResultList
;
get_all_basic_edges([EdgeListRecord| Tail], ResultList) ->
	if
		EdgeListRecord#edge.state == basic ->
			NewResultList = [ EdgeListRecord | ResultList],
			get_all_basic_edges(Tail, NewResultList);
			%% Edge != basic
		true->
			ok
	end	
.	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Get the Min Basic Edge 
get_min_basic_edge(EdgesList) ->
	
	AllBasicEdges = get_all_basic_edges(EdgesList, []),
	MinEdge = getMinEdge(AllBasicEdges),
	
	%% Return MinEdge
	MinEdge
.			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check if two Edges are Equal
%% Edges are Equal when they have the same Weight and the same NodePID ..NOT State			
proveEqual(Edge1, Edge2 )->
  if
    (Edge1 == undefined) or (Edge2 == undefined) ->
      false;
    true ->
      (Edge1#edge.weight_id == Edge2#edge.weight_id)
      and (Edge1#edge.node_pid == Edge2#edge.node_pid)
  end
.		
			
			
			
			
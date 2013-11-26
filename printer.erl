%%%-------------------------------------------------------------------
%%% @author Akcicek, Osman, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2013 10:05 AM
%%%-------------------------------------------------------------------
-module(printer).
-author("me").

%% API
-export([start/1]).






start(NumberOfNodes) ->


  global:register_name("printer", self()),


  receivePrintMessages(NumberOfNodes, "MST: ")
.



receivePrintMessages(NumberOfNodes, PrintList) ->

  if
  % all nodes did sent their print infos
    NumberOfNodes == 0 ->

      file:write_file("/home/me/Desktop/test/src1/MST.txt", PrintList);
    true ->
      receive
        {print, NodeID, InBranchOfNodeID} ->
          % NodeID of node that has got the InBranchOfNodeID, OutgoingNodeID of node that has NodeIDs InBranch as out-branch
          NewPrintList = lists:append([PrintList, "\n", atom_to_list(NodeID), " In Branch Edge ID: ", integer_to_list(InBranchOfNodeID)]),
          receivePrintMessages(NumberOfNodes - 1, NewPrintList)
      end


  end


.


%% buildPrintString([]) ->
%%
%%   ;
%%
%% buildPrintString(PrintList) ->
%%
%%   buildPrintString(PrintList, PrintString)
%%
%% ;
%%
%% buildPrintString([{NodeID, InBranchOfNodeID, OutgoingNodeID} | Tail], PrintString) ->
%%
%%
%%   .
%%
%%
%%
%% findNodeTupleWith(_, []) ->
%%   erlang:error("Error in printer:findeNodeTupleWith : No such Element...")
%% ;
%%
%%
%% findNodeTupleWith(WantedNodeID, [{NodeID, InBranchOfNodeID, OutgoingNodeID} | Tail]) ->
%%   if
%%     WantedNodeID == NodeID ->
%%       {NodeID, InBranchOfNodeID, OutgoingNodeID};
%%     true ->
%%       findTupleWith(NodeID, Tail)
%%   end
%% .


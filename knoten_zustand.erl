%%%-------------------------------------------------------------------
%%% @author Akcicek, Berngruber
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Nov 2013 1:33 PM
%%%-------------------------------------------------------------------
-module(knoten_zustand).
-author("Akcicek, Berngruber").

%% API
-export([start_knoten_zustand/1]).
-import(werkzeug, [get_config_value/2, logging/2]).



start_knoten_zustand(LoggingPID) ->

  LoggingPID ! {output, "Knoten Zustand: Starte Knoten Zustand..."},

  % Read out the config file.
  {ok, ConfigListe} = file:consult("node.cfg"),
  {ok, LocalNodeName} = get_config_value(nodename, ConfigListe),
  % initialize node
  NodeState = sleeping,
  NodeLevel = undefined,
  FindCount = undefined,
  FragmentID = undefined,


  mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID)
.



mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID) ->
  LoggingPID ! {output_knoten_zustand, "Knoten Zustand: in mainLoop..."},
  receive

    {get_level, NodeLogicPID} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> get_level..."},
      NodeLogicPID ! {node_level, NodeLevel},
      mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID);

    {get_node_state, NodeLogicPID} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> get_node_state..."},
      NodeLogicPID ! {node_state, NodeState},
      mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID);

    {get_fragment_id, NodeLogicPID} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> get_fragment_id..."},
      NodeLogicPID ! {fragment_id, FragmentID},
      mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID);

    {get_find_count, NodeLogicPID} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> get_find_count..."},
      NodeLogicPID ! {find_count, FindCount},
      mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, FragmentID, LoggingPID);


    {set_level, Value} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> set_level..."},
      mainLoop(LocalNodeName, NodeState, Value, FindCount, FragmentID, LoggingPID);

    {set_node_state, State} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> set_node_state..."},
      mainLoop(LocalNodeName, State, NodeLevel, FindCount, FragmentID, LoggingPID);

    {set_find_count, Value} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> set_find_count..."},
      mainLoop(LocalNodeName, NodeState, NodeLevel, Value, FragmentID, LoggingPID);

    {set_fragment_id, InboundFragmentId} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> set_fragment_id..."},
      mainLoop(LocalNodeName, NodeState, NodeLevel, FindCount, InboundFragmentId, LoggingPID);


    {find_count_plus_one} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> find_count_plus_one..."},
      NewFindCount = FindCount + 1,
      mainLoop(LocalNodeName, NodeState, NodeLevel, NewFindCount, FragmentID, LoggingPID);

    {find_count_minus_one} ->
      LoggingPID ! {output_knoten_zustand, "in mainLoop -> find_count_minus_one..."},
      NewFindCount = FindCount - 1,
      mainLoop(LocalNodeName, NodeState, NodeLevel, NewFindCount, FragmentID, LoggingPID)

  end


.

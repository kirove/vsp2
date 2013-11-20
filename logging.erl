% Logging File

-module(logging).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).
-import(werkzeug, [logging/2, timeMilliSecond/0]).


%% ====================================================================
%% Internal functions
%% ====================================================================


start(LogFileName) ->
  mainLoop(LogFileName, 0).


mainLoop(LogFileName, Counter) ->
  receive


  %% Server Logs
    {start} -> logging(LogFileName, "\n\n\n\n\n\n\n\nNode started\n");
    {pid, [ServiceName, PID]} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", ServiceName, ": ", pid_to_list(PID), "\n"]));
    {output, Output} -> logging(LogFileName, lists:append([integer_to_list(Counter), ": ", Output, "\n"]));
    {output_2_string_args, Output, Output2} -> logging(LogFileName, lists:append([integer_to_list(Counter), ": ", Output, " ", Output2,  "\n"]));
    {output_atom, Output, Atom} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", Output, " ", atom_to_list(Atom), "\n"]));
    {output_pid, Output, PID} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", Output, " ", pid_to_list(PID), "\n"]));
    {output_knoten, Output} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "=====KN ", Output, "\n"]));
    {output_receive, Output} -> logging(LogFileName, lists:append([integer_to_list(Counter), ": ", Output, "\n"]));
    {output_functions, Output} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "*****FU ", Output, "\n"]));
    {output_kanten_verwaltung, Output} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "+++++KV ", Output, "\n"]));
    {output_kanten_verwaltung_atom, Output, Atom} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "+++++KV ", Output, " :", atom_to_list(Atom), "\n"]));
    {output_knoten_zustand, Output} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "#####KZ ", Output, "\n"]));
    {send_connect_to, PID} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "Sending connect to: ", pid_to_list(PID), "\n"]));
    {output_send_to_node, Something, PID} ->
      logging(LogFileName, lists:append([integer_to_list(Counter), ": ", "Sending ", Something, " to ", pid_to_list(PID), "\n"]));
  % {output_send, Something} -> logging(LogFileName, lists:append(["Sending ", Something, "\n"]));
  % {output_requeue, Something} -> logging(LogFileName, lists:append(["Requeing ", Something, "\n"]));

    {list, ListName, List} ->
      logging(LogFileName, [integer_to_list(Counter), ": ", ListName, ": ", werkzeug:list2String(List), "\n"])


%%     {sendMsgID, [ClientPID, CurrentMessageID]} -> logging(LogFileName,lists:append(["Server received getmsid from Client ",pid_to_list(ClientPID),". Sending MsgID: ", integer_to_list(CurrentMessageID),"\n"]));
%%     {received_msg, [Message, MessageID]} -> logging(LogFileName,lists:append(["Server received dropmessage for MsgID: ",integer_to_list(MessageID)," Message: ", Message, "\n"]));
%%     {sending_msg, [ClientPID, Message, MessageID, TerminatedFlag]} -> logging(LogFileName,lists:append(["Server received getmessage from Client ",pid_to_list(ClientPID), " sending Message ", integer_to_list(MessageID), ": ", Message, "\n"]));
%%     {server_shutdown} -> logging(LogFileName,lists:append(["Server shutting down now...\n"]));
%%     %{push_to_DLQ, [MessageID, Message]} -> logging(LogFileName, ["Pushed message to DLQ: ", Message, " with ID: ", integer_to_list(MessageID), " into DLQ !\n"]);
%%
%%
%%     %% DEBUG
%%     {debug, [Log]} ->  logging(LogFileName, ["Server: ", Log, "\n"]);
%%     %queue
%%     {debug_int, [Log, Integer]} ->  logging(LogFileName, ["Queue: ", Log, integer_to_list(Integer), "\n"]);
%%     {debug_int_rest, [Log, Integer]} ->  logging(LogFileName, [Log, integer_to_list(Integer), "\n"]);
%%
%%     {debug_special, [Log, Integer]} ->  logging(LogFileName, ["Queue: ", Log, integer_to_list(Integer), "\n"]);
%%     {debug_queue, [Log]} ->  logging(LogFileName, ["QueueVerwaltung: ", Log, "\n"]);
%%     {log, MsgID, Message } -> logging(LogFileName , ["sending to client ",integer_to_list(MsgID), " Message: ", Message,"\n"]);
%%     {beautylist, ListName, List} ->  logging(LogFileName , [ListName, " " , werkzeug:list2String(List), "\n"])


  end,
  mainLoop(LogFileName, Counter + 1)
.
		

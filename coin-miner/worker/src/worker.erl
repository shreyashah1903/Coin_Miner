-module(worker).
-import(string,[equal/2]).
-export([start/2, connect/3]).
-export([mine_coin/1, mining_process_manager/3]).


for(0, _, Ids) ->
  Ids;
for(N, F, Process_Ids) ->
  Updated_process_Ids = [F() | Process_Ids],
  for(N-1, F, Updated_process_Ids).


initialise_processes(N, Init, Processes) ->
  if Init == true ->
    io:fwrite("Initializing processes.~n"),
    Process_count = 2,
    Process_ids = for(Process_count, fun() -> spawn(worker, mine_coin, [N]) end, []),
    Process_ids
  ;Init == false ->
    Processes
  end.


mining_process_manager(N, Init, Processes) ->
  Process_ids = initialise_processes(N, Init, Processes),
  receive
    start ->
      io:fwrite("Starting mining process.~n"),
      _ = [Process ! mine || Process <- Process_ids],
      mining_process_manager(N, false, Process_ids);
    terminate ->
      io:fwrite("Terminating process manager.~n"),
      _ = [Process ! terminate || Process <- Process_ids],
      io:fwrite("Process manager termination - success.~n")
  end.


mine_coin(N) ->
  receive
    mine ->
      Random_str = binary_to_list(base64:encode(crypto:strong_rand_bytes(8))),
      Input_key = string:concat("suriyan.subbaray;", Random_str),
      Sha256_digest = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Input_key))]),
      Regex = io_lib:format("^[0]{~p}",[N]),
      case re:run(Sha256_digest, Regex) of
        nomatch ->
          continue;
        {match, _} ->
          io:fwrite("matched~n"),
          connect_proc ! { sendResult, {Input_key, Sha256_digest}}
      end,
      self() ! mine;
    terminate ->
      io:fwrite("Termination of: ~p~n", [self()])
  end.


connect(Server_name, Node_name, ShouldConnect) ->
  Worker_id = self(),
  case equal(ShouldConnect, "true") of
    true -> { Server_name, Node_name } ! {workerReady, Worker_id};
    false -> ok
  end,
  receive
    { start, N } ->
      io:format("Start the worker with id ~p.~n", [Worker_id]),
      register(mining_process_manager_proc, spawn(worker, mining_process_manager, [N, true, []])),
      mining_process_manager_proc ! start;
    { sendResult, {Input_key, Sha256_digest}} ->
      io:format("Sending result to master ~p.~n", [Worker_id]),
      { mining_result_manager_proc, Node_name } ! {match, {Input_key, Sha256_digest}}
  end,
  connect(Server_name, Node_name, "false").


start(Server_name, Node_name) ->
  Connected = net_kernel:connect_node(Node_name),
  io:fwrite("Connected: ~p~n", [Connected]),
  register(connect_proc, spawn(worker, connect, [Server_name, Node_name, "true"])).

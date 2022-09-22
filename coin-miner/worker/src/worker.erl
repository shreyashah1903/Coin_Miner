-module(worker).
-import(string,[equal/2]).
-export([start/2, connect/3]).
-export([mine_coin/1, mining_process_manager/4, result_counter/2]).


for(0, _, Ids) ->
  Ids;
for(N, F, Process_Ids) ->
  Updated_process_Ids = [F() | Process_Ids],
  for(N-1, F, Updated_process_Ids).


initialise_processes(K, N, Init, Processes) ->
  if Init == true ->
    io:fwrite("Initializing processes.~n"),
    register(result_counter_proc, spawn(worker, result_counter, [N, 0])),
    Process_count = 20,
    Process_ids = for(Process_count, fun() -> spawn(worker, mine_coin, [K]) end, []),
    Process_ids
  ;Init == false ->
    Processes
  end.


mining_process_manager(K, N, Init, Processes) ->
  Process_ids = initialise_processes(K, N, Init, Processes),
  receive
    start ->
      io:fwrite("Starting mining process.~n"),
      _ = [Process ! mine || Process <- Process_ids],
      mining_process_manager(K, N, false, Process_ids);
    terminate ->
      io:fwrite("Terminating process manager.~n"),
      _ = [Process ! terminate || Process <- Process_ids],
      result_counter_proc ! terminate,
      io:fwrite("Process manager termination - success.~n")
  end.


mine_coin(K) ->
  receive
    mine ->
      Random_str = binary_to_list(base64:encode(crypto:strong_rand_bytes(8))),
      Members = ["suriyan.subbaray;", "shah.shreya;"],
      Random_index = rand:uniform(2),
      Input_key = string:concat(lists:nth(Random_index, Members), Random_str),
      Sha256_digest = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Input_key))]),
      Regex = io_lib:format("^[0]{~p}",[K]),
      case re:run(Sha256_digest, Regex) of
        nomatch ->
          continue;
        {match, _} ->
          result_counter_proc ! {process_result, {Input_key, Sha256_digest}}
      end,
      self() ! mine,
      mine_coin(K);
    terminate ->
      ok
  end.


result_counter(N, Curr_N) ->
  receive
    {process_result, {Input_key, Sha256_digest}} ->
      if
        Curr_N + 1 =< N ->
          connect_proc ! {sendResult, {Input_key, Sha256_digest}},
          if
            Curr_N + 1 == N ->
              connect_proc ! {notify_task_completion, N};
            Curr_N + 1 /= N ->
              continue
          end,
          result_counter(N, Curr_N + 1);
        Curr_N + 1 > N ->
          result_counter(N, Curr_N + 1)
      end;
    terminate ->
      ok
  end.

initialise_connection(Init, Server_name, Node_name, Worker_id) ->
  if Init == true ->
    {Server_name, Node_name} ! {worker_ready, Worker_id}
  ;Init == false ->
    ok
  end.

connect(Init, Server_name, Node_name) ->
  Worker_id = self(),
  initialise_connection(Init, Server_name, Node_name, Worker_id),
  receive
    {start, K, N} ->
      io:format("Start worker with K: ~p, N: ~p and id ~p.~n", [K, N, Worker_id]),
      register(mining_process_manager_proc, spawn(worker, mining_process_manager, [K, N, true, []])),
      mining_process_manager_proc ! start;
    {sendResult, {Input_key, Sha256_digest}} ->
      {mining_result_manager_proc, Node_name} ! {match, {Input_key, Sha256_digest}};
    {notify_task_completion, N} ->
      io:fwrite("Processing task completion signal.~n"),
      mining_process_manager_proc ! terminate,
      {Server_name, Node_name} ! {task_completed, Worker_id, N}
  end,
  connect(false, Server_name, Node_name).


start(Server_name, Node_name) ->
  Connected = net_kernel:connect_node(Node_name),
  io:fwrite("Connected: ~p~n", [Connected]),
  register(connect_proc, spawn(worker, connect, [true, Server_name, Node_name])).

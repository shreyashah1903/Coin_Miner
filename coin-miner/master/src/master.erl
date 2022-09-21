-module(master).
-import('./../../worker/src/worker', [start/2]).
-export([start/4, connect/3]).
-export([mining_result_manager/1, master_orchestrator/4]).


master_orchestrator(K, N, Server_name, Node_name) ->
  receive
    start ->
      register(mining_result_manager_proc, spawn(master, mining_result_manager, [[]])),
      register(master_connect, spawn(master, connect, [K, N, []])),
      worker:start(Server_name, Node_name),
      master_orchestrator(K, N, Server_name, Node_name);
    terminate ->
      mining_result_manager_proc ! terminate,
      mining_process_manager_proc ! terminate,
      master_connect ! terminate
  end.


mining_result_manager(Curr_result) ->
  receive
    {match, {Key, Sha256_digest}} ->
      Result = [{Key, Sha256_digest} | Curr_result],
      mining_result_manager(Result);
    display_results ->
      io:fwrite("Generated result is shown below:~n"),
      [io:fwrite("~p | ~p~n", [Key, Sha256]) || {Key, Sha256} <- Curr_result],
      mining_result_manager(Curr_result);
    terminate ->
      io:fwrite("Terminating result manager.~n"),
      [io:fwrite("~p | ~p~n", [Key, Sha256]) || {Key, Sha256} <- Curr_result],
      io:fwrite("Result manager termination - success.~n")
  end.

assignWork(N) ->
  MAX_WORK = 5,
  if
    N < MAX_WORK -> N;
    N >= MAX_WORK -> MAX_WORK
  end.

connect(K, N, Workers) ->
  io:fwrite("Master is running~n."),
  if
    N =< 0 ->
      master_orchestrator_proc ! terminate
  end,
  receive
    {worker_ready, Worker_id} ->
      io:fwrite("Sending signal to start worker with id ~p.~n", [Worker_id]),
      Work = assignWork(N),
      Workers = [Workers | Worker_id],
      Worker_id ! {start, K, Work},
      connect(K, N-Work, Workers);
    {task_completed, Worker_id} ->
      if
        N > 0 -> self() ! {worker_ready ! Worker_id};
        N =< 0 -> ok
      end;
    terminate ->
      io:fwrite("Terminating master connect process")
  end.


start(K, N, Server_name, Node_name) ->
  register(master_orchestrator_proc, spawn(master, master_orchestrator, [K, N, Server_name, Node_name])),
  master_orchestrator_proc ! start.

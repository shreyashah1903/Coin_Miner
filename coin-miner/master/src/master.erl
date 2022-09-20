-module(master).
-import('./../../worker/src/worker', [start/2]).
-export([start/3, connect/1]).
-export([mining_result_manager/1, master_orchestrator/3]).


master_orchestrator(N, Server_name, Node_name) ->
  receive
    start ->
      register(mining_result_manager_proc, spawn(master, mining_result_manager, [[]])),
      register(master_connect, spawn(master, connect, [N])),
      worker:start(Server_name, Node_name),
      master_orchestrator(N, Server_name, Node_name);
    terminate ->
      mining_result_manager_proc ! terminate,
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


connect(N) ->
  io:fwrite("Master is running~n."),
  receive
    {workerReady, Worker_id} ->
      io:fwrite("Sending signal to start worker with id ~p.~n", [Worker_id]),
      Worker_id ! {start, N},
      % TODO: Maintain Workers list
    connect(N);
    terminate ->
      io:fwrite("Terminating master connect process")
  end.


start(N, Server_name, Node_name) ->
  register(master_orchestrator_proc, spawn(master, master_orchestrator, [N, Server_name, Node_name])),
  master_orchestrator_proc ! start.

-module(master).
-define(NODE_WORKER, 'Worker1@saturn').
-export([start/1, connect/1]).
-export([mining_result_manager/1, master_orchestrator/1]).


master_orchestrator(N) ->
  receive
    start ->
      register(mining_result_manager_proc, spawn(master, mining_result_manager, [[]])),
      register(master, spawn(master, connect, [N])),
      master_orchestrator(N);
    terminate ->
      mining_result_manager_proc ! terminate
  end.


mining_result_manager(Curr_result) ->
  receive
    {match, {Key, Sha256_digest}} ->
      Result = [{Key, Sha256_digest} | Curr_result],
      mining_result_manager(Result);
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
      Worker_id ! {start, N}
      % TODO: Maintain Workers list
  end,
  connect(N).


start(N) ->
  register(master_orchestrator_proc, spawn(master, master_orchestrator, [N])),
  master_orchestrator_proc ! start.

-module(master).
-define(NODE_WORKER, 'Worker1@saturn').
-export([start/1, run/1]).

run(N) ->
  io:fwrite("Master running~n."),
  receive
    {workerReady, Worker_id} ->
      io:fwrite("Sending signal to start worker with id ~p.~n", [Worker_id]),
      Worker_id ! {start, N};
      % TODO: Maintain Workers list
    {result, Worker_id, Digest} ->
      io:fwrite("Result from worker ~p : digest : ~p~n", [Worker_id, Digest])
  end,
  run(N).


start(N) ->
  Process_id = register(master1, spawn(master, run, [N])),
  Process_id.
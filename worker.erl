-module(worker).
-define(NODE_SERVER, 'Node1@saturn').
-export([start/0, ping/0, mine/2, run/2]).

mine(N, Worker_id) ->
  io:format("Worker with id ~p started mining.~n", [N]),
  % If matches pattern send result to server
  { server1, ?NODE_SERVER } ! {result, Worker_id, "123"}.

ping() ->
  Worker_id = self(),
  { server1, ?NODE_SERVER } ! {workerReady, Worker_id},
  receive
    { start, N } ->
      io:format("Start the worker with id ~p.~n", [Worker_id]),
      Run_id = spawn(worker, run, [Worker_id, N]),
      Run_id ! {mine_coins, N, Worker_id},
      io:format("Start done.~n", [])
  end.


run(Worker_id, N) ->
  receive
    { stop, Worker_id } ->
      io:format("Stop the worker with id ~p~n", [Worker_id]);
    { mine_coins, N, Worker_id} ->
      io:format("Zeroes to find ~p.~n", [N]),
      mine(N, Worker_id),
      run(Worker_id, N)
  end.

start() ->
  io:format("Worker start called~n"),
  spawn(worker, ping, []).

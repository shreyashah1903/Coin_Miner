-module(worker).
-define(NODE_SERVER, 'Node1@saturn').
-export([start/2, mine/4, run/4, ping/2]).

mine(Server_name, Node_name, N, Worker_id) ->
  io:format("Worker with id ~p started mining.~n", [N]),
  % If matches pattern send result to server
  { Server_name, Node_name } ! {result, Worker_id, "123"}.

ping(Server_name, Node_name) ->
  Worker_id = self(),
  { Server_name, Node_name } ! {workerReady, Worker_id},
  receive
    { start, N } ->
      io:format("Start the worker with id ~p.~n", [Worker_id]),
      Run_id = spawn(worker, run, [Server_name, Node_name, Worker_id, N]),
      Run_id ! {mine_coins, N, Worker_id}
  end.


run(Server_name, Node_name, Worker_id, N) ->
  receive
    { stop, Worker_id } ->
      io:format("Stop the worker with id ~p~n", [Worker_id]);
    { mine_coins, N, Worker_id} ->
      io:format("Zeroes to find ~p.~n", [N]),
      mine(Server_name, Node_name, N, Worker_id),
      run(Server_name, Node_name, Worker_id, N)
  end.

start(Server_name, Node_name) ->
  io:format("Worker start called~n"),
  spawn(worker, ping, [Server_name, Node_name]).

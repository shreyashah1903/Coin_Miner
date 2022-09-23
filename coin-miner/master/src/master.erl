-module(master).
-import('./../../worker/src/worker', [start/2]).
-export([start/3, worker_nodes_manager/5]).
-export([mining_result_manager/1, master_node_manager/3]).

%%
%% @doc master_node_manager spawns management processes of master.
%%
%% master_node_manager function spawns and registers processes that
%% are used by the master node to manage the system. Its inputs
%% parameters K and N are used by the connection management process
%% Node_name is used to start a worker. In addition, it handles the
%% termination of these processes as a part of its termination.
%%

master_node_manager(K, N, Node_name) ->
  receive
    start ->
      register(mining_result_manager_proc, spawn(master, mining_result_manager, [[]])),
      register(worker_nodes_manager_proc, spawn(master, worker_nodes_manager, [K, N, N, 0, sets:new()])),
      worker:start(Node_name),
      master_node_manager(K, N, Node_name);
    terminate ->
      io:fwrite("Master node manager termination request.~n", []),
      mining_result_manager_proc ! terminate,
      worker_nodes_manager_proc ! terminate
  end.


%%
%% @doc mining_result_manager maintains the result of mining.
%%
%% mining_result_manager function manages the results it mines
%% and the ones it gets them from the workers. It is started as
%% a part of the node manager when the master node is initialised.
%% 1. {match, {Key, Sha256_digest}} signal receives a mined coin
%% and adds it to a list
%% 2. display_results signal is used to print the current state
%% of the result to the io
%% 3. terminate signal prints the results and exits the process.
%%

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


%%
%% @doc compute_work_unit returns the work unit per worker
%%
%% compute_work_unit is a util function used by the
%% worker_nodes_manager to limit the work unit per node to
%% a maximum threshold.
%%
compute_work_unit(N) ->
  MAX_WORK = 10,
  if
    N < MAX_WORK -> Work = N;
    N >= MAX_WORK -> Work = MAX_WORK
  end,
  Work.

%%
%% @doc worker_nodes_manager manages the connection and work assignment to the workers.
%%
%% worker_nodes_manager function accepts a worker when it requests to join the system.
%% In addition it keeps track of the remaining work and assigns work to each of the
%% workers as keep requesting for work until the total work is completed.
%%
%% @inputs
%% 1. K - number of zeros that each coin should at least start with.
%% 2. N - total number of coins to be generated (total work).
%% 3. Remaining_work - number of coins that are to be mined yet (work left).
%% 4. Total_work_done - number of coins to mined so far (completed work).
%% 5. Curr_workers - current set of workers that are part of the system.
%%
%% @signals
%% {worker_ready, Worker_id} - worker requests to join the system with this signal.
%% master maintains the worker_id that it receives in the signal and assigns work if
%% are any left.
%% {task_completed, Worker_id, Work_done} - worker notifies the master with this signal
%% when it completes the assigned work unit
%% terminate - when the master is terminating, it sends termination signal to the workers.
%%

worker_nodes_manager(K, N, Remaining_work, Total_work_done, Curr_workers) ->
  % io:fwrite("Master is running.~n"),
  if (Remaining_work =< 0) and (Total_work_done >= N) -> master_node_manager_proc ! terminate;
    (Remaining_work =< 0) and (Total_work_done < N) -> ok;
    Remaining_work > 0 -> ok
    %true -> ok
  end,
  io:fwrite("Remaining Work: ~p, Total work: ~p, Totalwork_done: ~p~n", [Remaining_work, N, Total_work_done]),
  receive
    {worker_ready, Worker_id} ->
      io:fwrite("Sending signal to start worker with id ~p.~n", [Worker_id]),
      Work = compute_work_unit(Remaining_work),
      Workers = sets:add_element(Worker_id, Curr_workers),
      Worker_id ! {start, K, Work},
      worker_nodes_manager(K, N, Remaining_work-Work, Total_work_done, Workers);
    {task_completed, Worker_id, Work_done} ->
      if
        Remaining_work > 0 -> self() ! {worker_ready, Worker_id};
        Remaining_work =< 0 -> ok
      end,
      worker_nodes_manager(K, N, Remaining_work, Total_work_done + Work_done, Curr_workers);
    terminate ->
      io:fwrite("Terminating master connect process and all worker nodes.~n"),
      _ = [ Id ! shutdown || Id <- sets:to_list(Curr_workers)]
  end.


%%
%% @doc start is the entry point to the master
%%
%% start function initializes a master node by registering and
%% starting a node manager process
%% 1. K - number of zeros that each coin should at least start with
%% 2. N - total number of coins to be mined. This is the total work
%% that the system should accomplish together. Each worker node (including
%% the master) will be continuously assigned a chunk of this total work
%% until the total work is accomplished.
%% 3. Master_node_name is the name assigned to the master node.
%%

start(K, N, Master_node_name) ->
  register(master_node_manager_proc, spawn(master, master_node_manager, [K, N, Master_node_name])),
  master_node_manager_proc ! start.

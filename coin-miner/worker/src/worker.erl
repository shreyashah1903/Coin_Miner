-module(worker).
-import(string,[equal/2]).
-export([start/1, connection_manager/3]).
-export([mine_coin/1, mining_process_manager/4, result_counter/2]).

%%
%% @doc for creates a given number of processes.
%%
%% for function executes an anonymous function, that it gets as a
%% argument, which spawns a process. for repeats this until process_count
%% becomes 0 and returns the process_ids of all the spawned processes.
%%

for(0, _, Process_Ids) ->
  Process_Ids;
for(Process_count, Function, Process_Ids) ->
  Updated_process_Ids = [Function() | Process_Ids],
  for(Process_count - 1, Function, Updated_process_Ids).


%%
%% @doc initialise_processes initialises the mining processes.
%%
%% initialise_processes function spawns the mining processes when the worker
%% has been assigned a work. In addition it spawns result_counter_proc that is
%% used to keep track of the number of coins mined.
%%

initialise_processes(K, N, Init, Processes) ->
  if Init == true ->
    io:fwrite("Initializing processes.~n"),
    register(result_counter_proc, spawn(worker, result_counter, [N, 0])),
    Process_count = 15,
    Process_ids = for(Process_count, fun() -> spawn(worker, mine_coin, [K]) end, []),
    Process_ids
  ;Init == false ->
    Processes
  end.


%%
%% @doc mining_process_manager manages the lifecycle of the mining processes.
%%
%% mining_process_manager spawns the mining processes using initialise_processes
%% when the worker is given a work.
%% 1. When mining_process_manager receives a start signal, it starts all the
%% mining processes that it spawned.
%% 2. terminate signal sends termination message to each of the mining process and the
%% result counter process.
%%

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


%%
%% @doc mine_coin mines a coin and sends to result_counter.
%%
%% mine_coin function mines a coin in three steps. First it forms a input key to
%% sha256 algo. It does this by generating a random string and appending it to
%% one of the members' name. Second, it passes the above key to the sha256 algo
%% and gets a digest a value. Finally, it matches the digest with a regex to check
%% if the digest at least K number of zeros at the beginning. If the digest does
%% contain at least K zeros, then it sends the match to the result counter.
%%

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


%%
%% @doc result_counter keeps a count of the number of coins mined.
%%
%% result counter function receives mined coins from the processes.
%% It keeps a count on the number of coins mined and sends the coin
%% to the connection_manager_proc.
%%
%%

result_counter(Total_coins, Coins_mined) ->
  receive
    {process_result, {Input_key, Sha256_digest}} ->
      if Coins_mined + 1 =< Total_coins ->
          connection_manager_proc ! {sendResult, {Input_key, Sha256_digest}},
          if Coins_mined + 1 == Total_coins ->
              connection_manager_proc ! {notify_task_completion, Total_coins}
          ;Coins_mined + 1 /= Total_coins ->
              continue
          end,
          result_counter(Total_coins, Coins_mined + 1)
      ;Coins_mined + 1 > Total_coins ->
          result_counter(Total_coins, Coins_mined + 1)
      end;
    terminate ->
      ok
  end.


%%
%% @doc initialise_connection function sends a ready signal to the master.
%%

initialise_connection(Init, Server_name, Node_name, Worker_id) ->
  if Init == true ->
    {Server_name, Node_name} ! {worker_ready, Worker_id}
  ;Init == false ->
    ok
  end.

%%
%% @doc connection_manager manages sending and receiving messages to master
%%
%% connection_manager function starts the request to the master to join the mining
%% system.
%% 1. {start, K, N} - It receives a response signal with values for K (number of zeros in coin)
%% and N (work_unit - number of coins to be mined). Then, it starts the process_manager which
%% starts the mining processes.
%% 2. {notify_task_completion, N} - This signal is sent by the the result_counter when
%% the total work is completed.
%% 3. shutdown signal is received from the master during its termination. It also prints the
%% cpu and real time and cpu to real time ratio since the worker started.
%%

connection_manager(Init, Server_process_name, Server_node_name) ->
  Worker_id = self(),
  initialise_connection(Init, Server_process_name, Server_node_name, Worker_id),
  receive
    {start, K, N} ->
      io:format("Start worker with K: ~p, N: ~p and id ~p.~n", [K, N, Worker_id]),
      register(mining_process_manager_proc, spawn(worker, mining_process_manager, [K, N, true, []])),
      mining_process_manager_proc ! start;
    {sendResult, {Input_key, Sha256_digest}} ->
      {mining_result_manager_proc, Server_node_name} ! {match, {Input_key, Sha256_digest}};
    {notify_task_completion, N} ->
      io:fwrite("Processing task completion signal.~n"),
      mining_process_manager_proc ! terminate,
      {Server_process_name, Server_node_name} ! {task_completed, Worker_id, N};
    shutdown ->
      {_, Cpu_time_since_start} = statistics(runtime),
      {_, Wall_time_since_start} = statistics(wall_clock),
      io:fwrite("-----cpu time during worker end: ~p. ~n", [Cpu_time_since_start]),
      io:fwrite("-----wall clock time during worker end: ~p. ~n", [Wall_time_since_start]),
      io:fwrite("cpu-time to real-time :: ~p ~n", [Cpu_time_since_start/Wall_time_since_start]),
      io:fwrite("Shutdown signal received for worker ~p.~n", [self()]),
      exit(self(), normal)
  end,
  connection_manager(false, Server_process_name, Server_node_name).


%%
%% @doc start initializes a worker node
%%
%% In start function, worker node establishes a connection to the master node
%% and spawns the connection manager process. In addition, it contains calls to
%% statistics() function which will help getting cpu and real time when the worker ends.
%%

start(Server_node_name) ->
  {_, _} = statistics(runtime),
  {_, _} = statistics(wall_clock),
  Connected = net_kernel:connect_node(Server_node_name),
  io:fwrite("Connection result: ~p~n", [Connected]),
  Server_process_name = worker_nodes_manager_proc,
  register(connection_manager_proc, spawn(worker, connection_manager, [true, Server_process_name, Server_node_name])).

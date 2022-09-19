%%%-------------------------------------------------------------------
%%% @author Suriyan Subbarayan
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Sep 2022 8:29 PM
%%%-------------------------------------------------------------------
-module(master).
-author("Suriyan").

%% API
-export([run/1, mine_coin/1, mining_result_manager/1, master_orchestrator/1, mining_process_manager/2]).

run(N) ->
  register(master_orchestrator_proc, spawn(master, master_orchestrator, [N])),
  master_orchestrator_proc ! start.

master_orchestrator(N) ->
  receive
    start ->
      register(mining_result_manager_proc, spawn(master, mining_result_manager, [[]])),
      register(mining_process_manager_proc, spawn(master, mining_process_manager, [N, true])),
      mining_process_manager_proc ! start,
      master_orchestrator(N);
    terminate ->
      mining_process_manager_proc ! terminate,
      mining_result_manager_proc ! terminate
  end.

for(0, _, Ids) ->
  Ids;
for(N, F, Process_Ids) ->
  Updated_process_Ids = [F() | Process_Ids],
  for(N-1, F, Updated_process_Ids).

initialise_processes(_, false) ->
  ok;
initialise_processes(N, true) ->
  io:fwrite("Initializing processes.~n"),
  Process_count = 10,
  Process_ids = for(Process_count, fun() -> spawn(master, mine_coin, [N]) end, []),
  Process_ids.

mining_process_manager(N, Init) ->
  Process_ids = initialise_processes(N, Init),
  io:fwrite("Processes ~p. ~n", [Process_ids]),
  receive
    start ->
      io:fwrite("Starting mining process.~n"),
      _ = [Process ! mine || Process <- Process_ids];
    terminate ->
      io:fwrite("Terminating process manager.~n"),
      _ = [Process ! terminate || Process <- Process_ids],
      io:fwrite("Process manager termination - success.~n")
  end,
  mining_process_manager(N, false).

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
          mining_result_manager_proc ! {match, {Input_key, Sha256_digest}}
      end,
      self() ! mine;
    terminate ->
      io:fwrite("Termination of: ~p~n", [self()])
  end.








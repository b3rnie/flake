%%%
%%%_* Module declaration ===============================================
-module(flake_SUITE).

%%%_* Exports ==========================================================
%% ct
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% tc
-export([ types_test/1
        , sequential_ids_test/1
        , parallell_test/1
        , clock_backwards_test/1
        , clock_advanced_test/1
        , rw_timestamp_test/1
        , time_server_died_test/1
        ]).

%%%_* Code =============================================================

all() ->
    [ types_test
    , sequential_ids_test
    , parallell_test
    , clock_backwards_test
    , clock_advanced_test
    , rw_timestamp_test
    , time_server_died_test
    ].

init_per_suite(Config) ->
  ok = application:load(flake),
  {ok, [File, Downtime, Interface, Interval]} =
    flake_util:get_env([ timestamp_file
                       , allowable_downtime
                       , interface
                       , interval]),
  [{timestamp_file, File}, {allowable_downtime, Downtime},
   {interface, Interface}, {interval, Interval} | Config].
end_per_suite(Config) ->
  application:unload(flake),
  Config.

init_per_testcase(TC, Config) when TC =:= types_test;
                                   TC =:= sequential_ids_test;
                                   TC =:= parallell_test ->
  ok = application:start(flake),
  Config;
init_per_testcase(clock_backwards_test, Config) ->
  File = kf(timestamp_file, Config),
  Ts   = flake_util:now_in_ms() + 5000,
  ok   = flake_util:write_timestamp(File, Ts),
  Config;
init_per_testcase(clock_advanced_test, Config) ->
  File     = kf(timestamp_file, Config),
  Downtime = kf(allowable_downtime, Config),
  Ts       = flake_util:now_in_ms() - Downtime - 1,
  ok       = flake_util:write_timestamp(File, Ts),
  Config;
init_per_testcase(rw_timestamp_test, Config) ->
  file:delete(kf(timestamp_file, Config)),
  Config;
init_per_testcase(time_server_died_test, Config) ->
  erlang:process_flag(trap_exit, true),
  {ok, Pid1} = flake_time_server:start_link([]),
  {ok, Pid2} = flake_server:start_link([]),
  [{flake_time_server_pid, Pid1}, {flake_server_pid, Pid2} | Config];
init_per_testcase(_TC, Config) ->
  Config.

end_per_testcase(time_server_died_test, Config) ->
  exit(kf(flake_time_server_pid, Config), killed),
  exit(kf(flake_server_pid, Config), killed),
  file:delete(kf(timestamp_file, Config)),
  ok;
end_per_testcase(_TC, Config) ->
  application:stop(flake),
  file:delete(kf(timestamp_file, Config)),
  ok.

%%%_ *  ----------------------------------------------------------------
types_test(_Config) ->
  {ok, Bin} = flake:id_bin(),
  {ok, Str} = flake:id_str(10),
  {ok, Int} = flake:id_int(),

  true = erlang:is_binary(Bin),
  true = erlang:is_list(Str),
  true = erlang:is_integer(Int),
  ok.

%%%_ *  ----------------------------------------------------------------
sequential_ids_test(_Config) ->
  F = fun(_N, {Int0, Int1}) when Int0 < Int1 ->
          {Int1, flake:id_int()}
      end,
  lists:foldl(F, {flake:id_int(), flake:id_int()}, lists:seq(1, 10000)),
  ok.

%%%_ *  ----------------------------------------------------------------
-define(processes, 16).
-define(requests, 5000).
parallell_test(_Config) ->
  Pids  = [start_worker(self()) || _N <- lists:seq(1, ?processes)],
  Res   = [receive {Pid, Ids} -> Ids end || Pid <- Pids],
  ?processes * ?requests = length(lists:usort(lists:concat(Res))),
  ok.

start_worker(Daddy) ->
  proc_lib:spawn_link(fun() -> run_worker(Daddy, ?requests, []) end).

run_worker(Daddy, 0, Acc) -> Daddy ! {self(), Acc};
run_worker(Daddy, N, Acc) ->
  {ok, Bin} = flake:id_bin(),
  run_worker(Daddy, N-1, [Bin|Acc]).

%%%_ *  ----------------------------------------------------------------
clock_backwards_test(_Config) ->
  erlang:process_flag(trap_exit, true),
  {error, clock_running_backwards} = flake_time_server:start_link([]),
  ok.

%%%_ *  ----------------------------------------------------------------
clock_advanced_test(_Config) ->
  erlang:process_flag(trap_exit, true),
  {error, clock_advanced} = flake_time_server:start_link([]),
  ok.

%%%_ *  ----------------------------------------------------------------
rw_timestamp_test(Config) ->
  File            = kf(timestamp_file, Config),
  Ts0             = 0,
  Ts1             = 1,
  {error, enoent} = flake_util:read_timestamp(File),
  ok              = flake_util:write_timestamp(File, Ts0),
  {ok, Ts0}       = flake_util:read_timestamp(File),
  ok              = flake_util:write_timestamp(File, Ts1),
  {ok, Ts1}       = flake_util:read_timestamp(File),
  ok.

%%%_ *  ----------------------------------------------------------------
time_server_died_test(Config) ->
  Interval      = kf(interval, Config),
  TimeserverPid = kf(flake_time_server_pid, Config),

  {ok, _Bin1} = flake:id_bin(),
  exit(TimeserverPid, killed),
  {ok, _Bin2} = flake:id_bin(),
  timer:sleep(Interval),
  {ok, _Bin3} = flake:id_bin(),
  timer:sleep(Interval),
  {error, clock_advanced} = flake:id_bin(),
  ok.

%%%_ *  ----------------------------------------------------------------

kf(K, L) ->
  {K, V} = lists:keyfind(K, 1, L),
  V.
%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

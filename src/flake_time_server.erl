%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc saves timestamps with configurable interval.
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(flake_time_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ start_link/1
        , get_last_ts/0
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-record(s, { file
           , tref
           , last_ts
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_last_ts() ->
  gen_server:call(?MODULE, get_last_ts).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  {ok, File}     = application:get_env(flake, timestamp_file),
  {ok, Downtime} = application:get_env(flake, allowable_downtime),
  Now            = flake_util:now_in_ms(),
  case flake_util:read_timestamp(File) of
    {ok, Ts} ->
      if Ts > Now            -> {stop, clock_running_backwards};
         Now - Ts > Downtime -> {stop, clock_advanced};
         true                -> {ok, do_init(File, Now)}
      end;
    {error, enoent} -> {ok, do_init(File, Now)};
    {error, Rsn}    -> {stop, Rsn}
  end.

handle_call(get_last_ts, _From, #s{last_ts = Ts} = S) ->
  {reply, Ts, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(save, #s{ last_ts = Ts
                    , file    = File} = S) ->
  Now = flake_util:now_in_ms(),
  case Now >= Ts of
    true  -> flake_util:write_timestamp(File, Now),
             {noreply, S#s{last_ts = Now}};
    false -> {stop, clock_running_backwards, S}
  end;

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
do_init(File, Now) ->
  flake_util:write_timestamp(File, Now),
  {ok, Interval} = application:get_env(flake, interval),
  {ok, TRef} = timer:send_interval(Interval, save),
  #s{ file    = File
    , tref    = TRef
    , last_ts = Now
    }.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

clock_backwards_test() ->
  File = load_and_write(flake_util:now_in_ms() + 5000),
  %% startup will fail, dont want to kill test..
  erlang:process_flag(trap_exit, true),
  {error, clock_running_backwards} = flake_time_server:start_link([]),
  file:delete(File),
  ok.

clock_advanced_test() ->
  application:load(flake),
  {ok, Downtime} = application:get_env(flake, allowable_downtime),
  File = load_and_write(flake_util:now_in_ms() - Downtime - 1),
  erlang:process_flag(trap_exit, true),
  {error, clock_advanced} = flake_time_server:start_link([]),
  file:delete(File).

load_and_write(Ts) ->
  application:load(flake),
  {ok, File} = application:get_env(flake, timestamp_file),
  flake_util:write_timestamp(File, Ts),
  File.

periodic_save_test() ->
  File = load_and_write(flake_util:now_in_ms()),
  {ok, Pid} = flake_time_server:start_link([]),
  timer:sleep(2000),
  exit(normal, Pid),
  ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

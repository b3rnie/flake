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
           , interval
           , ts
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_persisted_ts() ->
  gen_server:call(?MODULE, get_persisted_ts).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  case flake_util:get_env([ timestamp_file
                          , allowable_downtime
                          , interval]) of
    {ok, [File, Downtime, Interval]} ->
      Now = flake_util:now_in_ms(),
      case flake_util:read_timestamp(File) of
        {ok, Ts} when Ts > Now ->
          {stop, clock_running_backwards};
        {ok, Ts} when Now - Ts > Downtime ->
          {stop, clock_advanced};
        {ok, Ts} ->
          Delay = Ts + Interval - Now + 1,
          maybe_delay(Delay),
          do_init(Now, #s{file = File, interval = Interval});
        {error, enoent} ->
          do_init(Now, #s{file = File, interval = Interval});
        {error, Rsn} ->
          {stop, Rsn}
      end;
    {error, Rsn} ->
      {stop, Rsn}
  end.

handle_call(get_persisted_ts, _From, #s{ts = Ts} = S) ->
  {reply, Ts, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(save, #s{file = File, ts = Ts0} = S) ->
  case update_persisted_ts(File, Ts0) of
    {ok, Ts}     -> %% flake_server:set_last_persisted_ts(Ts),
                    {noreply, S#s{ts = Ts}};
    {error, Rsn} -> {stop, Rsn, S}
  end;

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------

do_init(Now, #s{file = File, interval = Interval} = S) ->
  case update_persisted_ts(File, Now) of
    {ok, NewTs} ->
      {ok, TRef} = timer:send_interval(Interval, save),
      {ok, S#s{ts = NewTs}};
    {error, Rsn} ->
      {stop, Rsn}
  end.

maybe_delay(Delay) when Delay > 0 ->
  error_logger:info_msg("delaying startup: ~p", [Delay]),
  timer:sleep(Delay);
maybe_delay(_Delay) -> ok.

update_persisted_ts(File, OldTs) ->
  case flake_util:now_in_ms() of
    Ts when Ts < OldTs -> {error, clock_running_backwards};
    Ts                 -> flake_util:write_timestamp(File, Ts),
                          {ok, Ts}
  end.

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
  _File = load_and_write(flake_util:now_in_ms()),
  {ok, Pid} = flake_time_server:start_link([]),
  timer:sleep(2000),
  exit(Pid, normal),
  ok.

-else.
-endif.


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

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
        , get_ts/0
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
-record(s, { file     :: list()
           , interval :: integer()
           , tref
           , ts       :: integer()
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_ts() ->
  gen_server:call(?MODULE, get_ts, infinity).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  case flake_util:get_env([ timestamp_file
                          , allowable_downtime
                          , interval]) of
    {ok, [File, Downtime, Interval]} ->
      Now = flake_util:now_in_ms(),
      error_logger:info_msg("~p: now = ~p~n", [?MODULE, Now]),
      case flake_util:read_timestamp(File) of
        {ok, Ts} ->
          error_logger:info_msg("~p: last persisted ts = ~p~n",
                                [?MODULE, Ts]),
          if Ts > Now ->
              {stop, clock_running_backwards};
             Now - Ts > Downtime ->
              {stop, clock_advanced};
             true ->
              maybe_delay(Ts + Interval * 2 - Now),
              do_init(Now, #s{file = File, interval = Interval})
          end;
        {error, enoent} ->
          do_init(Now, #s{file = File, interval = Interval});
        {error, Rsn} ->
          {stop, Rsn}
      end;
    {error, Rsn} ->
      {stop, Rsn}
  end.

handle_call(get_ts, _From, #s{ts = Ts} = S) ->
  {reply, Ts, S}.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(save, #s{file = File, ts = Ts0} = S) ->
  case update_persisted_ts(File, Ts0) of
    {ok, Ts}     -> flake_server:update_persisted_ts(Ts),
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
      {ok, S#s{ts = NewTs, tref = TRef}};
    {error, Rsn} ->
      {stop, Rsn}
  end.

maybe_delay(Delay) when Delay > 0 ->
  error_logger:info_msg("~p: delaying startup = ~p~n", [?MODULE, Delay]),
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


-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2012
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(flake_time_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ start_link/1
        , write_timestamp/1
        , read_timestamp/1
        , get_last_timestamp/1
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
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_last_timestamp() ->
    gen_server:call(?MODULE, get_last_timestamp).

%% write the current time stamp to disk
%% {ok,Timestamp=int()} | {error,Reason}
write_timestamp(Table) ->
    TS = flake_util:now_in_ms(),
    ok = dets:insert(Table,{last_timestamp,TS}),
    {ok,TS}.

%% read the timestamp from the given file. will write the current timestamp to disk if the file does not exist
%% {ok,Timestamp=int()} | {error,Reason}
read_timestamp(Table) ->
    case dets:lookup(Table,last_timestamp) of
	[{last_timestamp,TS}] when is_integer(TS) ->
	    {ok,TS};
	_ ->
	    write_timestamp(Table)
    end.

get_clock_state() ->
        TimestampPath = flake:get_config_value(timestamp_path, "/tmp/flake-timestamp-dets"),
    AllowableDowntime = flake:get_config_value(allowable_downtime, 0),

    {ok, TimestampTable} =
	dets:open_file(timestamp_table,[
					{estimated_no_objects, 10},
					{type, set},
					{file, TimestampPath}
				       ]),

    {ok,TS} = persistent_timer:read_timestamp(TimestampTable),
    ?debugVal(TS),
    Now = flake_util:curr_time_millis(),
    ?debugVal(Now),
    TimeSinceLastRun = Now - TS,

    %% fail startup if
    %% 1) the clock time last recorded is later than the current time
    %% 2) the last recorded time is more than N ms in the past to prevent
    %%    generating future ids in the event that the system clock is set to some point far in the future
    check_for_clock_error(Now >= TS, TimeSinceLastRun < AllowableDowntime),

    error_logger:info_msg("saving timestamps to ~p every 1s~n", [TimestampPath]).



check_for_clock_error(true,true) ->
    ok;
check_for_clock_error(false,_) ->
    error_logger:error_msg("system running backwards, failing startup of snowflake service~n"),
    exit(clock_running_backwards);
check_for_clock_error(_,false) ->
    error_logger:error_msg("system clock too far advanced, failing startup of snowflake service~n"),
    exit(clock_advanced).

%%%_ * gen_server callbacks --------------------------------------------
init(Args) ->
  F = fun({file, File}, S)         -> S#s{file = File};
         ({interval, Interval}, S) -> S#s{interval = Interval};
         ({_K, _V}, S)             -> S
      end,
  S = lists:foldl(F, #s{}, Args),
  {ok, TRef} = timer:send_interval(S#s.interval, save),
  {ok, S#s{tref = TRef}}.

handle_call(get_last_timestamp, _From, #state{table=Table} = S) ->
  {reply, read_timestamp(Table), S}.

handle_cast(_Msg, S) -> {noreply, S}.

handle_info(save, #s{file = File} = S) ->
  ok = flake_util:write_timestamp(File),
  {noreply, S};

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

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

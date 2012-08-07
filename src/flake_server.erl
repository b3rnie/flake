%%%
%%% Copyright 2012, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% Refactored 2012 / Bjorn Jensen-Urstad

%%%_* Module declaration ===============================================
-module(flake_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([ start_link/1
        , id/0
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
-record(s, { interval          = undefined              :: integer()
           , last_persisted_ts = undefined              :: integer()
           , last_used_ts      = flake_util:now_in_ms() :: integer()
           , last_used_seqno   = 0                      :: integer()
           , mac_addr          = undefined              :: binary()
           }).

%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc generate a new snowflake id
id() -> gen_server:call(?MODULE, get).

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  {ok, Interface} = application:get_env(flake, interface),
  {ok, Interval}  = application:get_env(flake, interval),
  case flake_util:get_mac_addr(Interface) of
    {ok, MacAddr} ->
      {ok, Ts} = flake_time_server:subscribe(),
      {ok, #s{ mac_addr          = MacAddr
             , interval          = Interval
             , last_persisted_ts = Ts
             }};
    {error, Rsn} ->
      {stop, Rsn}
  end.

terminate(_Rsn, _S) ->
  flake_time_server:unsubsribe(),
  
  ok.

handle_call(get, _From, #s{ last_used_ts      = LastTs
                          , last_used_seqno   = LastSeqno
                          , mac_addr          = MacAddr
                          , last_persisted_ts = LastPersistedTs
                          , interval          = Interval
                          } = S0) ->
  case flake_util:now_in_ms() of
    Now when Now - (LastPersistedTs + Interval * 2) >= 0 ->
      %% cant generate id, current time too far ahead of persisted.
      %% If we do there is a risk of generating duplicates.
      {reply, {error, clock_advanced}, S0};
    Now ->
      case next(LastTs, Now, LastSeqno) of
        {ok, {Ts, Seqno}} ->
          S = S0#s{last_used_ts = Ts, last_used_seqno = Seqno},
          {reply, {ok, flake_util:mk_id(Ts, MacAddr, Seqno)}, S};
        {error, out_of_seqno} ->
          {reply, {error, out_of_seqno}, S0};
        {error, Rsn} ->
          {stop, Rsn, {error, Rsn}, S0}
      end
  end.

handle_cast(stop, S) ->
  {stop, normal, S}.

handle_info({ts, Ts}, #s{last_persisted_ts = LastPersistedTs} = S) ->
  io:format("NEW TS: ~p~n", [Ts]),
  io:format("OLD TS: ~p~n", [LastPersistedTs]),
  case Ts >= LastPersistedTs of
    true  -> {noreply, S#s{last_persisted_ts = Ts}};
    false -> {stop, clock_running_backwards, S}
  end;

handle_info(_Info, S) ->
  {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
next(OldTs, OldTs, Seqno) when Seqno >= 16#FFFF ->
  {error, out_of_seqno};
next(OldTs, OldTs, Seqno) ->
  {ok, {OldTs, Seqno+1}};
next(OldTs, NewTs, _Seqno) when OldTs < NewTs ->
  {ok, {NewTs, 0}};
next(OldTs, NewTs, _Seqno) when OldTs > NewTs ->
  {error, clock_running_backwards}.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

next_test() ->
  Ts0 = flake_util:now_in_ms(),
  Ts1 = Ts0 + 1,
  {ok, {Ts0, 1}} = next(Ts0, Ts0, 0),
  {ok, {Ts0, 2}} = next(Ts0, Ts0, 1),
  {ok, {Ts1, 0}} = next(Ts0, Ts1, 0),
  {ok, {Ts1, 0}} = next(Ts0, Ts1, 1),
  {error, clock_running_backwards} = next(Ts1, Ts0, 0),
  {error, clock_running_backwards} = next(Ts1, Ts0, 1),
  ok.

next_out_of_seqno_test() ->
  Ts = flake_util:now_in_ms(),
  {error, out_of_seqno} = next(Ts, Ts, 16#FFFF),
  ok.

non_existing_mac_addr_test() ->
  flake_test:test_init(),
  {ok, Interface} = application:get_env(flake, interface),
  application:set_env(flake, interface, dummy),
  erlang:process_flag(trap_exit, true),
  {error, interface_not_found} = start_link([]),
  application:set_env(flake, interface, Interface),
  flake_test:test_end().

%% test that flake_server stops giving out id's
%% after flake_time_server dies
time_server_died_test() ->
  flake_test:test_init(),
  erlang:process_flag(trap_exit, true),
  {ok, Interval} = application:get_env(flake, interval),
  {ok, Pid1} = flake_time_server:start_link([]),
  {ok, Pid2} = flake_server:start_link([]),
  {ok, _Bin1} = flake:id_bin(),
  exit(Pid1, die),
  {ok, _Bin2} = flake:id_bin(),
  timer:sleep(Interval),
  {ok, _Bin3} = flake:id_bin(),
  timer:sleep(Interval),
  {error, clock_advanced} = flake:id_bin(),
  {error, clock_advanced} = flake:id_bin(),
  exit(Pid2, die),
  timer:sleep(1),
  flake_test:test_end().

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

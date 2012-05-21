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
        , id/1
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
-record(s, { last_ts  :: integer()
           , mac_addr :: integer()
           , seqno    :: integer()
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc generate a new snowflake id
id() ->
  gen_server:call(?MODULE, get).

id(Base) ->
  case gen_server:call(?MODULE, get) of
    {ok, <<Id:128/integer>>} ->
      {ok, flake_util:integer_to_list(Id, Base)};
    {error, Rsn} ->
      {error, Rsn}
  end.

%%%_ * gen_server callbacks --------------------------------------------
init(_Args) ->
  case application:get_env(flake, interface) of
    {ok, Interface} ->
      {ok, MacAddr} = flake_util:get_mac_addr(Interface),
      {ok, #s{ last_ts  = flake_time_server:get_last_ts()
             , mac_addr = flake_util:mac_addr_to_int(MacAddr)
             , seqno    = 0
             }};
    undefined ->
      {stop, no_interface_configured}
  end.

handle_call(get, _From, #s{ last_ts  = LastTs
                          , mac_addr = MacAddr
                          , seqno    = Seqno0
                          } = S0) ->
  case next(LastTs, flake_util:now_in_ms(), Seqno0) of
    {ok, {Ts, Seqno}}     -> S   = S0#s{last_ts = Ts, seqno = Seqno},
                             Res = flake_util:mk_id(Ts, MacAddr, Seqno),
                             {reply, {ok, Res}, S};
    {error, out_of_seqno} -> {reply, {error, out_of_seqno}, S0};
    {error, Rsn}          -> {stop, Rsn, S0}
  end.

handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info(_Info, S) ->
  {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
  timer:cancel(TRef),
  ok.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%%%_ * Internals -------------------------------------------------------
next(OldTs, OldTs, Seqno) when Seqno >= 65535 -> %% 16 bits
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
  <<Int:16/integer>> = <<16#FF, 16#FF>>,
  {error, out_of_seqno} = next(Ts, Ts, Int),
  ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

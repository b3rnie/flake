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
-record(s, { tref
           , last_ts  :: integer()
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
  {ok, Interface} = application:get_env(interface),
  {ok, MacAddr    = flake_util:get_max_addr(Interface),
  {ok, #s{ last_ts  = flake_time_server:get_last_ts(),
         , mac_addr = flake_util:mac_addr_to_int(Mac)
         , seqno    = 0
         }}.

handle_call({get, Format}, _From, #s{ last_ts  = LastMs
                                    , mac_addr = MacAddr
                                    , seqno    = Seqno0
                                    } = S0) ->
  case next(LastMs, flake_util:now_in_ms(), Seqno0) of
    {ok, {NowMs, Seqno}}  -> S   = S0#s{last_ts = NowMs, seqno = Seqno},
                             Res = flake_util:mk_id(Ts, MacAddr, Seqno),
                             {reply, Res, S};
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
next(Old, Old, Seqno) when Seqno >= 65536 -> %% 16 bits
  {error, out_of_seqno};
next(Old, Old, Seqno) ->
  {ok, {Old, Seqno+1}};
next(Old, New, _Seqno) when Old < New ->
  {ok, {New, 0}};
next(Old, New, _Seqno) when Old > New ->
  {error, clock_running_backwards}.

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

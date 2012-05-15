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
-author ('Dietrich Featherston <d@boundary.com>').
%%% Refactored 2012 / Bjorn Jensen-Urstar <bjorn.urstad@klarna.com>

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
           , max_time
           , worker_id
           , sequence :: integer()
           }).
%%%_ * API -------------------------------------------------------------
start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% @doc generate a new snowflake id
id()     -> gen_server:call(?MODULE, get).
id(Base) -> gen_server:call(?MODULE, {get, Base}).

%%%_ * gen_server callbacks --------------------------------------------
init([{worker_id, WorkerId}]) ->
  {ok, #s{ max_time=flake_util:now_in_ms()
         , worker_id=WorkerId
         , sequence=0}}.

handle_call(get, _From, #s{ max_time = MaxTime
                          , worker_id = WorkerId
                          , sequence = Sequence
                          } = S0) ->
    {Resp, S1} = get(flake_util:now_in_ms(), MaxTime, WorkerId,
                     Sequence, S0),
    {reply, Resp, S1};


handle_call({get, Base}, _From, #s{ max_time = MaxTime
                                  , worker_id = WorkerId
                                  , sequence  = Sequence
                                  } = S0) ->
    {Resp, S1} = get(flake_util:now_in_ms(), MaxTime, WorkerId,
                     Sequence, S0),
    case Resp of
	{ok, <<IntId:128/integer>>} ->
	    {reply, {ok, flake_util:as_list(IntId, Base)}, S0};
	E ->
	    {reply, E, S0}
    end.

handle_cast(_Msg, S) -> {noreply, S}.

handle_info(_Info, S) -> {noreply, S}.

terminate(_Rsn, #s{tref = TRef}) ->
    timer:cancel(TRef),
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%_ * Internals -------------------------------------------------------

%% clock hasn't moved, increment sequence
get(Time,Time,WorkerId,Seq0,S) ->
    Sequence = Seq0 + 1,
    {{ok,flake_util:gen_id(Time,WorkerId,Sequence)},S#s{sequence=Sequence}};
%% clock has progressed, reset sequence
get(CurrTime,MaxTime,WorkerId,_,S) when CurrTime > MaxTime ->
  {{ok, flake_util:gen_id(CurrTime, WorkerId, 0)}, S#s{max_time=CurrTime, sequence=0}};
%% clock is running backwards
get(CurrTime, MaxTime, _WorkerId, _Sequence, S) when MaxTime > CurrTime ->
  {{error, clock_running_backwards}, S}.

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

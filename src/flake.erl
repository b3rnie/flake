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
-module (flake).

%%%_* Exports ==========================================================
-export([ id/0
        , id/1
        ]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc id() -> binary()
id() -> flake_server:id().

%% @doc id(Base::integer()) -> list()
id(Base) ->
  case flake_server:id(Base) of
    {ok, <<Id:128/integer>>} ->
      {ok, flake_util:integer_to_list(Id, Base)};
    {error, _Rsn} = Err ->
      Err
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% genereate bunch of ids in parallell and and make sure they are unique.
-define(processes, 5).
-define(requests, 1000).
parallell_5k_ids_test() ->
  ok    = application:start(flake, permanent),
  Pids  = [start_worker(self()) || _N <- lists:seq(1, ?processes)],
  Res   = [receive {Pid, Ids} -> Ids end || Pid <- Pids],
  ?processes * ?requests = length(lists:usort(lists:concat(Res))),
  application:stop(flake),
  flake_test_lib:cleanup(),
  ok.

start_worker(Daddy) ->
  proc_lib:spawn_link(fun() -> run_worker(Daddy, ?requests, []) end).

run_worker(Daddy, 0, Acc) -> Daddy ! {self(), Acc};
run_worker(Daddy, N, Acc) ->
  {ok, <<Int:128/integer>>} = flake:id(),
  run_worker(Daddy, N-1, [Int|Acc]).

%% ids should be sequential
sequential_ids_test() ->
  ok = application:start(flake),
  ok = next(flake:id(), 500),
  ok = application:stop(flake),
  flake_test_lib:cleanup(),
  ok.

next(_, 0) -> ok;
next({ok, <<PrevInt:128/integer>>}, N) ->
  {ok, <<NextInt:128/integer>>} = Next = flake:id(),
  true = PrevInt < NextInt,
  next(Next, N-1).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

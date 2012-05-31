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
-export([ id_bin/0
        , id_str/1
        , id_int/0
        ]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------

%% @doc uid in binary format
id_bin() ->
  flake_server:id().

%% @doc uid in string format
id_str(Base) ->
  case flake_server:id() of
    {ok, <<Id:128/integer>>} ->
      {ok, flake_util:integer_to_list(Id, Base)};
    {error, Rsn} ->
      {error, Rsn}
  end.

%% @doc uid in integer format
id_int() ->
  case flake_server:id() of
    {ok, <<Id:128/integer>>} -> {ok, Id};
    {error, Rsn}             -> {error, Rsn}
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

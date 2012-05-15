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
-module (flake_sup).
-behaviour(supervisor).

%%%_* Exports ==========================================================
-export([ start_link/0
        , init/1
        ]).

%%%_* Code =============================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  RestartStrategy = {one_for_all, 0, 1},
  Kids = [ {flake_time_server, {flake_time_server, start_link, [TimeConf]},
            permanent, 5000, worker, [flake_time_server]}
         , {flake_server, {flake_server, start_link, [Config]},
            permanent, 5000, worker, [flake_server]}
         ],
  {ok, {RestartStrategy, Kids}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

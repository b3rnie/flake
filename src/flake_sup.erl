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
  %% 1. always generate unique id's
  %% 2. be fast and efficient
  %%
  %% These conflict with eachother when we need to handle restarts.
  %% In order to never generate duplicates we need to persist
  %% timestamps used to generate flake id's to disk which obviously
  %% is a big performance hit.
  %%
  %% flake_time_server persists the current time to disk with regular
  %% (configurable) intervals.
  %% ----------|----------|----------|--------       persists
  %%         write      write      write
  %% -----|-----|---|---|-----|-----|-----|          requests
  %%    req   req req  req  req   req   req
  %%
  %% flake_server is using timestamps not persisted to disk
  %% when calculating id's so there might exist a gap where duplicate
  %% id's can be generated if flake_server is restarted in a shorter
  %% time than the persist interval and the clock is stopped
  %% or running backwards. This is handled by delaying
  %% startup to guarantee unique id's.
  RestartStrategy = {one_for_all, 4, 60},
  Kids = [ {flake_time_server, {flake_time_server, start_link, [[]]},
            permanent, 5000, worker, [flake_time_server]}
         , {flake_server, {flake_server, start_link, [[]]},
            permanent, 5000, worker, [flake_server]}
         ],
  {ok, {RestartStrategy, Kids}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

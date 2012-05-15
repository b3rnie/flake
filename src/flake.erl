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
-module (flake).

%%%_* Exports ==========================================================
-export([ id/0
        , id/1
        , get_config_value/2
        ]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
id()     -> flake_server:id().
id(Base) -> flake_server:id(Base).

get_config_value(Key, Default) ->
    case application:get_env(flake, Key) of
	{ok, Value} -> Value;
	_ -> Default
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

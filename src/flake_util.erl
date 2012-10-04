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
-module (flake_util).
-compile({no_auto_import, [integer_to_list/2]}).

%%%_* Exports =========================================================
-export([ mk_id/3
        , now_in_ms/0
        , get_mac_addr/1
        , integer_to_list/2
        , get_env/2
        , default_env/0
        ]).

%%%_* Includes =========================================================
-include_lib("flake/include/flake.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc mac addr as a 6 byte binary
get_mac_addr(_Interface = any) ->
  {ok, Addrs} = inet:getifaddrs(),
  %% try to skip loopback
  case lists:filter(fun({_Interface, Props}) ->
                        case lists:keyfind(hwaddr, 1, Props) of
                          {hwaddr, [0,0,0,0,0,0]} -> false;
                          {hwaddr, _Mac}          -> true;
                          false                   -> false
                        end
                    end, Addrs) of
    [{_If, Props}|_] ->
      {hwaddr, Mac} = lists:keyfind(hwaddr, 1, Props),
      {ok, erlang:list_to_binary(Mac)};
    [] ->
      %% NOTE: do we really want this as a fallback?
      {ok, <<0,0,0,0,0,0>>}
  end;
get_mac_addr(Interface) ->
  {ok, Addrs} = inet:getifaddrs(),
  case lists:keyfind(Interface, 1, Addrs) of
    {Interface, Props} ->
      case lists:keyfind(hwaddr, 1, Props) of
        {hwaddr, Mac} -> {ok, erlang:list_to_binary(Mac)};
        false         -> {error, mac_not_found}
      end;
    false ->
      {error, interface_not_found}
  end.

now_in_ms() ->
  {Ms, S, Us} = erlang:now(),
  1000000000*Ms + S*1000 + erlang:trunc(Us/1000).

mk_id(Ts, Mac, Seqno)
  when erlang:is_integer(Ts),
       erlang:is_binary(Mac),
       erlang:is_integer(Seqno) ->
  <<Ts:64/integer, Mac:6/binary, Seqno:16/integer>>.

%%
%% n.b. - unique_id_62/0 and friends pulled from riak
%%
%% @doc Convert an integer to its string representation in the given
%%      base.  Bases 2-62 are supported.
integer_to_list(I, Base)
  when erlang:is_integer(I),
       erlang:is_integer(Base),
       Base >= 2,
       Base =< 1+$Z-$A+10+1+$z-$a -> %% 62
  case I < 0 of
    true  -> [$-|integer_to_list(-I, Base, [])];
    false -> integer_to_list(I, Base, [])
  end.

integer_to_list(I0, Base, R0) ->
  R1 = case I0 rem Base of
         D when D >= 36 -> [D-36+$a|R0];
         D when D >= 10 -> [D-10+$A|R0];
         D              -> [D+$0   |R0]
       end,
  case I0 div Base of
    0  -> R1;
    I1 -> integer_to_list(I1, Base, R1)
  end.

get_env(Key, Def) ->
  case application:get_env(flake, Key) of
    {ok, Val} -> Val;
    undefined -> Def
  end.

default_env() ->
  [ {interface,          ?interface}
  , {interval,           ?interval}
  , {timestamp_path,     ?timestamp_path}
  , {allowable_downtime, ?allowable_downtime}
  ].

%%%_* Tests ============================================================
-ifdef(TEST).

id_test() ->
  Ts    = now_in_ms(),
  Mac   = erlang:list_to_binary(lists:seq(1, 6)),
  Seqno = 0,
  Flake = mk_id(Ts, Mac, Seqno),
  <<Ts:64/integer, Mac:6/binary, Seqno:16/integer>> = Flake,
  ok.

integer_to_list_base2_32_test() ->
  %% TODO: Use proper for this
  lists:foreach(fun(N) ->
                    %% Random base between 2 and 32
                    Base = random:uniform(31) + 1,
                    I    = case N rem 2 of
                             0 ->  random:uniform(16#FFFF);
                             1 -> -random:uniform(16#FFFF)
                           end,
                    R0 = integer_to_list(I, Base),
                    R1 = erlang:integer_to_list(I, Base),
                    ?assertEqual(R0, R1)
                end, lists:seq(1, 500)).

integer_to_list_base62_test() ->
  "A"   = integer_to_list(10, 62),
  "H"   = integer_to_list(17, 62),
  "1c"  = integer_to_list(100, 62),
  "G8"  = integer_to_list(1000, 62),
  "2bI" = integer_to_list(10000, 62),
  ok.

any_interface_test() ->
  {ok, <<_Mac:6/binary>>} = get_mac_addr(any),
  ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

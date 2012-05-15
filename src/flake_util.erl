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

-module (flake_util).
-author ('Dietrich Featherston <d@boundary.com>').

-export ([
	  as_list/2,
	  get_if_hw_int/1,
	  hw_addr_to_int/1,
         , now_in_ms/0

	  curr_time_millis/0,
	  gen_id/3
	 ]).

-include_lib("eunit/include/eunit.hrl").

%% get the mac/hardware address of the given interface as a 48-bit integer
get_if_hw_int(undefined) ->
    {error, if_not_found};
get_if_hw_int(IfName) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    IfProps = proplists:get_value(IfName, IfAddrs),
    case IfProps of
	undefined ->
	    {error, if_not_found};
	_ ->
	    HwAddr = proplists:get_value(hwaddr, IfProps),
	    {ok, hw_addr_to_int(HwAddr)}
    end.

%% convert an array of 6 bytes into a 48-bit integer
hw_addr_to_int(HwAddr) ->
    <<WorkerId:48/integer>> = erlang:list_to_binary(HwAddr),
    WorkerId.

now_in_ms() ->
  {Ms, S, Us} = erlang:now(),
  1000000000*Ms + S*1000 + erlang:trunc(Us/1000).

gen_id(Time,WorkerId,Sequence) ->
    <<Time:64/integer, WorkerId:48/integer, Sequence:16/integer>>.

%%
%% n.b. - unique_id_62/0 and friends pulled from riak
%%

%% @spec integer_to_list(Integer :: integer(), Base :: integer()) ->
%%          string()
%% @doc Convert an integer to its string representation in the given
%%      base.  Bases 2-62 are supported.
as_list(I, 10) ->
    erlang:integer_to_list(I);
as_list(I, Base)
  when is_integer(I),
       is_integer(Base),
       Base >= 2,
       Base =< 1+$Z-$A+10+1+$z-$a -> %% 62
  if
      I < 0 ->
	  [$-|as_list(-I, Base, [])];
      true ->
	  as_list(I, Base, [])
  end;
as_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

%% @spec integer_to_list(integer(), integer(), stringing()) -> string()
as_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 =
	if
	    D >= 36 ->
		[D-36+$a|R0];
	    D >= 10 ->
		[D-10+$A|R0];
	    true ->
		[D+$0|R0]
	end,
    if
      I1 =:= 0 ->
	    R1;
	true ->
	    as_list(I1, Base, R1)
    end.

write_timestamp(File, Ts) ->
  file:write_file(File, erlang:term_to_binary(Ts)).

read_timestamp(File) ->
  case file:read_file(File) of
    {ok, Bin}       -> {ok, erlang:binary_to_term(Bin)};
    {error, enoent} -> {error, enoent};
    {error, Rsn}    -> {error, Rsn}
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(tmpfile, "/tmp/flake.tmp").
timestamp_test() ->
  file:delete(?tmpfile),
  Ts = now_in_ms(),
  {error, enoent} = read_timestamp(?tmpfile),
  ok              = write_timestamp(?tmpfile, Ts),
  {ok, Ts}        = read_timestamp(?tmpfile),
  ok.

flake_test() ->
    TS = flake_util:now_in_ms(),
    Worker = flake_util:hw_addr_to_int(lists:seq(1, 6)),
    Flake = flake_util:gen_id(TS, Worker, 0),
    <<Time:64/integer, WorkerId:48/integer, Sequence:16/integer>> = Flake,
    ?assert(?debugVal(Time) =:= TS),
    ?assert(?debugVal(Worker) =:= WorkerId),
    ?assert(?debugVal(Sequence) =:= 0),
    <<FlakeInt:128/integer>> = Flake,
    ?debugVal(flake_util:as_list(FlakeInt, 62)),
    ok.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

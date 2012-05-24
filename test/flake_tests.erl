%%%_* Module declaration ===============================================
-module(flake_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% genereate bunch of ids in parallell and and make sure
%% they are unique.

-define(processes, 5).
-define(requests, 1000).
paralell_5k_ids_test() ->
  ok    = application:start(flake, permanent),
  Daddy = self(),

  F = fun(_N) ->
          proc_lib:spawn_link(fun() -> worker(Daddy, ?requests) end)
      end,
  Workers = lists:map(F, lists:seq(1, ?processes)),
  Res     = lists:map(fun(Pid) ->
                          receive {Pid, Ids} ->
                              Ids
                          end
                      end, Workers),
  ?processes * ?requests = length(lists:usort(lists:concat(Res))),
  application:stop(flake),
  ok.

worker(Daddy, N) -> worker(Daddy, N, []).

worker(Daddy, 0, Acc) -> Daddy ! {self(), Acc};
worker(Daddy, N, Acc) ->
  {ok, <<Int:128/integer>>} = flake:id(),
  worker(Daddy, N-1, [Int|Acc]).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

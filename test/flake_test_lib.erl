%%%
%%%
%%%_* Module declaration ===============================================
-module(flake_test_lib).

%%%_* Exports ==========================================================
-ifdef(TEST).
-export([ cleanup/0
        ]).

%%%_* Code =============================================================
cleanup() ->
  {ok, File} = application:get_env(flake, timestamp_file),
  file:delete(File).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

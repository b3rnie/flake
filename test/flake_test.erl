-module(flake_test).

-ifdef(TEST).
-export([ test_init/0
        , test_end/0
        , until_unregistered/1
        ]).

test_init() ->
  application:load(flake),
  {ok, Path} = application:get_env(flake, timestamp_path),
  ok = filelib:ensure_dir(filename:join([Path, "dummy"])),
  file:delete(flake_time_server:real_file(Path)),
  ok.

test_end() ->
  {ok, Path} = application:get_env(flake, timestamp_path),
  file:delete(flake_time_server:real_file(Path)),
  ok.

until_unregistered(Name) ->
  case erlang:whereis(Name) of
      undefined -> ok;
      _ -> timer:sleep(1),
           until_unregistered(Name)
  end.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

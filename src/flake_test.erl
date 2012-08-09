-module(flake_test).

-export([ test_init/0
        , test_end/1
        , until_unregistered/1
        ]).

test_init() ->
  application:load(flake),
  application:stop(flake),
  {ok, Path} = application:get_env(flake, timestamp_path),
  ok = filelib:ensure_dir(filename:join([Path, "dummy"])),
  file:delete(flake_time_server:real_file(Path)),
  application:get_all_env(flake).

test_end(Oldenv) ->
  lists:foreach(fun({K,V}) ->
                    application:set_env(flake, K, V)
                end, Oldenv),
  {ok, Path} = application:get_env(flake, timestamp_path),
  file:delete(flake_time_server:real_file(Path)),
  application:start(flake),
  ok.

until_unregistered(Name) ->
  case erlang:whereis(Name) of
      undefined -> ok;
      _ -> timer:sleep(1),
           until_unregistered(Name)
  end.


%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

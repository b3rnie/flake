%%%_* Module declaration ===============================================
-module(flake_test).

%%%_* Exports ==========================================================
-export([ with_env/1
        , with_env/2
        , clean_env/1
        , clean_env/2
        , wait_unregistered/1
        ]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
with_env(Fun) ->
  in_env(flake_util:default_env(), Fun, true).

with_env(Env, Fun) ->
  in_env(Env, Fun, true).

clean_env(Fun) ->
  in_env(flake_util:default_env(), Fun, false).

clean_env(Env, Fun) ->
  in_env(Env, Fun, false).

in_env(Env, Fun, Start) ->
  OldEnv  = application:get_all_env(flake),
  Running = application:which_applications(),
  try
    stop_app(flake),
    setenv(flake, Env, OldEnv),
    cleanup(),
    [ok = application:start(flake) || Start],
    Fun(),
    stop_app(flake)
  after
    setenv(flake, OldEnv, Env),
    cleanup(),
    maybe_start(Running)
  end.

stop_app(App) ->
  application:stop(App),
  case application:get_key(App, registered) of
    {ok, Names} -> wait_unregistered(Names);
    undefined   -> ok
  end.

wait_unregistered([N|Ns]) ->
  case whereis(N) of
    undefined -> wait_unregistered(Ns);
    _         -> wait_unregistered([N|Ns])
  end;
wait_unregistered([]) -> ok.

setenv(App, Env, OldEnv) ->
  UnsetF = fun({K,_}) -> application:unset_env(App, K) end,
  SetF   = fun({K,V}) -> application:set_env(App, K, V) end,
  lists:foreach(UnsetF, OldEnv),
  lists:foreach(SetF,   Env).

cleanup() ->
  case application:get_env(flake, timestamp_path) of
    {ok, Path} ->
      ok = filelib:ensure_dir(filename:join([Path, "dummy"])),
      file:delete(flake_time_server:real_file(Path));
    undefined ->
      ok
  end.

maybe_start(Running) ->
  case lists:keyfind(flake, 1, Running) of
    {flake, _, _} -> ok = application:start(flake);
    false         -> ok
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

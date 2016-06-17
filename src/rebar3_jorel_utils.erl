-module(rebar3_jorel_utils).

-export([jorel_app/2, jorel_config/1]).

-define(JOREL_APP, bucfile:expand_path("~/.jorel/jorel")).
-define(JOREL_APP_MASTER, bucfile:expand_path("~/.jorel/jorel.master")).
-define(JOREL_URL, "https://github.com/emedia-project/jorel/wiki/jorel").
-define(JOREL_MASTER_URL, "https://github.com/emedia-project/jorel/wiki/jorel.master").
-define(JOREL_CONFIG, "jorel.config").

jorel_app(Master, Upgrade) ->
  {JorelURL, JorelApp} = if
                           Master -> {?JOREL_MASTER_URL, ?JOREL_APP_MASTER};
                           true -> {?JOREL_URL, ?JOREL_APP}
                         end,
  JorelDir = filename:dirname(JorelApp),
  case (not filelib:is_file(JorelApp)) orelse Upgrade of
    true ->
      ssl:start(),
      inets:start(),
      rebar_api:info("Download ~s", [JorelURL]),
      case httpc:request(get, {JorelURL, []}, [{autoredirect, true}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
          case filelib:is_dir(JorelDir) of
            false ->
              bucfile:make_dir(JorelDir);
            true ->
              ok
          end,
          case file:write_file(JorelApp, Body) of
            ok -> 
              ok;
            {error, Reason} ->
              rebar_api:abort("Can't save ~s: ~p", [JorelApp, Reason])
          end,
          file:change_mode(JorelApp, 8#00755);
        _ ->
          rebar_api:abort("Faild to download Jorel!", [])
      end;
    false ->
      ok
  end,
  rebar_api:info("Use ~s", [JorelApp]),
  JorelApp.

jorel_config(State) ->
  rebar_api:info("Generate ~s", [?JOREL_CONFIG]),
  case rebar_state:get(State, jorel, []) of
    [] ->
      rebar_api:abort("Missing jorel configuration (see http://jorel.in/installation/#rebar3-plugin)", []);
    Config ->
      file:write_file(?JOREL_CONFIG, "", [write]),
      lists:foreach(fun(C) ->
                        file:write_file(?JOREL_CONFIG, io_lib:fwrite("~p.~n", [C]), [append])
                    end, Config),
      ?JOREL_CONFIG
  end.


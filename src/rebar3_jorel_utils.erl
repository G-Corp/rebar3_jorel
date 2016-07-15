-module(rebar3_jorel_utils).

-export([jorel_app/2, jorel_config/1, jorel_cmd/2]).

-define(JOREL_APP, bucfile:expand_path("~/.jorel/jorel")).
-define(JOREL_APP_MASTER, bucfile:expand_path("~/.jorel/jorel.master")).
-define(JOREL_URL, "https://github.com/emedia-project/jorel/wiki/jorel").
-define(JOREL_MD5_URL, "https://github.com/emedia-project/jorel/wiki/jorel.md5").
-define(JOREL_MASTER_URL, "https://github.com/emedia-project/jorel/wiki/jorel.master").
-define(JOREL_MASTER_MD5_URL, "https://github.com/emedia-project/jorel/wiki/jorel.master.md5").
-define(JOREL_CONFIG, "jorel.config").

jorel_app(Master, Upgrade) ->
  {JorelURL, JorelMD5URL, JorelApp} = if
                                        Master -> {?JOREL_MASTER_URL,
                                                   ?JOREL_MASTER_MD5_URL,
                                                   ?JOREL_APP_MASTER};
                                        true -> {?JOREL_URL,
                                                 ?JOREL_MD5_URL,
                                                 ?JOREL_APP}
                                      end,
  JorelDir = filename:dirname(JorelApp),
  case (not filelib:is_file(JorelApp)) orelse Upgrade of
    true ->
      ssl:start(),
      inets:start(),
      case check(JorelApp, JorelMD5URL) of
        true -> 
          todo;
        false ->
          case httpc:request(get, {JorelURL, []}, [{autoredirect, true}], []) of
            {ok, {{_, 200, _}, _, Body}} ->
              rebar_api:info("Upgrade ~s", [JorelApp]),
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
              rebar_api:abort("Failed to download Jorel!", [])
          end
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

jorel_cmd(State, Command) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  Upgrade = proplists:get_value(upgrade, Args, false),
  Master = proplists:get_value(master, Args, false),
  Force = proplists:get_value(force, Args, false),

  JorelApp = rebar3_jorel_utils:jorel_app(Master, Upgrade),

  KeepConfig = filelib:is_file(?JOREL_CONFIG),
  JorelConfig = case (not KeepConfig) orelse Force of
                  true ->
                    rebar3_jorel_utils:jorel_config(State);
                  false ->
                    rebar_api:warn("~s exist, use it. (use --force to override)", [?JOREL_CONFIG]),
                    ?JOREL_CONFIG
                end,
  rebar_api:info("Execute ~s", [JorelApp]),
  Cmd = string:join([JorelApp|Command], " "),
  rebar_utils:sh(Cmd,
                 [use_stdout, {cd, rebar_state:dir(State)}, {abort_on_error, "Jorel failed"}]),
  if
    KeepConfig -> ok;
    true -> file:delete(JorelConfig)
  end,
  {ok, State}.

check(JorelApp, JorelMD5URL) ->
  crypto:start(),
  case filelib:is_file(JorelApp) of
    true ->
      case file:read_file(JorelApp) of
        {ok, Data} ->
          case httpc:request(get, {JorelMD5URL, []}, [{autoredirect, true}], []) of
            {ok, {{_, 200, _}, _, MD5}} ->
              string:to_lower(
                binary_to_list(
                  encode16(
                    crypto:hash(md5, Data)))) == string:strip(MD5, both, $\n);
            _ ->
              false
          end;
        _ ->
          false
      end;
    false ->
      false
  end.

encode16(Data) ->
  << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= Data >>.

hex(N) when N < 10 ->
      N + $0;
hex(N) when N < 16 ->
      N - 10 + $a.


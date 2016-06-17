-module(rebar3_jorel_release).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, release).
-define(DEPS, [{default, compile}]).

-define(JOREL_CONFIG, "jorel.config").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, rebar_state:add_provider(
         State, 
         providers:create(
           [{name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, jorel},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 jorel release"},
            {short_desc, "Generate a release with Jorel"},
            {desc, "Generate a release with Jorel."},
            {opts, [
                    {upgrade, $u, "upgrade", undefined, "Upgrade jorel (default : false)"},
                    {force, $f, "force", undefined, "Force rewrite jorel.config (default : false)"},
                    {master, $M, "master", undefined, "Use jorel master (default : false)"}
                   ]
            }]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
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
  rebar_utils:sh(JorelApp,
                 [use_stdout, {cd, rebar_state:dir(State)}, {abort_on_error, "Jorel faild"}]),
  if
    KeepConfig -> ok;
    true -> file:delete(JorelConfig)
  end,
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



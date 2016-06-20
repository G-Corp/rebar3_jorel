-module(rebar3_jorel_gen_config).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, gen_config).
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
            {example, "rebar3 jorel gen_config"},
            {short_desc, "Generate a configuration file for Jorel"},
            {desc, "Generate a configuration file for Jorel."},
            {opts, [
                    {force, $f, "force", undefined, "Force rewrite jorel.config (default : false)"}
                   ]
            }]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  Force = proplists:get_value(force, Args, false),

  case (not filelib:is_file(?JOREL_CONFIG)) orelse Force of
    true ->
      rebar3_jorel_utils:jorel_config(State);
    false ->
      rebar_api:warn("~s exist. (use --force to override)", [?JOREL_CONFIG])
  end,
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



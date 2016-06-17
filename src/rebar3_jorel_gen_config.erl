-module(rebar3_jorel_gen_config).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, gen_config).
-define(DEPS, [{default, compile}]).

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
                    {output, $o, "output", undefined, "Output file (default : jorel.config)"}
                   ]
            }]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Dir = rebar_state:dir(State),
  rebar_api:info("---> ~p", [Dir]),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



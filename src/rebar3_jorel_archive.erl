-module(rebar3_jorel_archive).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, archive).
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
            {example, "rebar3 jorel archive"},
            {short_desc, "Create archive of release"},
            {desc, "Create archive of release."},
            {opts, [
                    {upgrade, $u, "upgrade", undefined, "Upgrade jorel (default : false)"},
                    {force, $f, "force", undefined, "Force rewrite jorel.config (default : false)"},
                    {master, $M, "master", undefined, "Use jorel master (default : false)"}
                   ]
            }]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar3_jorel_utils:jorel_cmd(State, "archive").

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



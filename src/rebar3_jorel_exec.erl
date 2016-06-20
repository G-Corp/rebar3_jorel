-module(rebar3_jorel_exec).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, exec).
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
            {example, "rebar3 exec providers"},
            {short_desc, "Execute a specific Jorel command"},
            {desc, "Execute a specific Jorel command."},
            {opts, []}
           ]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Args = rebar_state:command_args(State),
  rebar_api:warn("===> ~p ", [Args]),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).



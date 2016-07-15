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
            {example, "rebar3 exec provider"},
            {short_desc, "Execute a specific Jorel command"},
            {desc, "Execute a specific Jorel command."},
            {opts, [
                    {upgrade, $u, "upgrade", undefined, "Upgrade jorel (default : false)"},
                    {master, $M, "master", undefined, "Use jorel master (default : false)"}
                   ]}
           ]))}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Args0 = lists:delete("--", rebar_state:command_args(State)),
  {Upgrade, Args1} = args("--upgrade", "-u", Args0),
  {Master, Args2} = args("--master", "-M", Args1),
  JorelApp = rebar3_jorel_utils:jorel_app(Master, Upgrade),
  Cmd = string:join([JorelApp|Args2], " "),
  rebar_api:info("Execute ~s", [Cmd]),
  rebar_utils:sh(Cmd,
                 [use_stdout, {cd, rebar_state:dir(State)}, {abort_on_error, "Jorel failed"}]),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

args(Long, Short, Args) ->
  {R, Args0} = case lists:member(Long, Args) of
                 true ->
                   {true, lists:delete(Long, Args)};
                 false ->
                   {false, Args}
               end,
  case lists:member(Short, Args0) of
    true ->
      {true, lists:delete(Short, Args0)};
    false ->
      {R, Args0}
  end.


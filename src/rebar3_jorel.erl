-module(rebar3_jorel).

-export([init/1]).

init(State) ->
  lists:foldl(fun provider_init/2, {ok, State}, [rebar3_jorel_release
                                                 , rebar3_jorel_gen_config
                                                 , rebar3_jorel_exec]).

provider_init(Module, {ok, State}) ->
  Module:init(State).


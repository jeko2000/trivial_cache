-module(tc_app).

-export([start/2, stop/1]).

-behaviour(application).

start(_StartType, _StartArgs) ->
    tc_store:init(),
    tc_sup:start_link().

stop(_State) ->
    ok.

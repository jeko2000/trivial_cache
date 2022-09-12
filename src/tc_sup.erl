-module(tc_sup).

-export([start_link/0, start_child/2]).

-export([init/1]).

-behaviour(supervisor).

-define(SERVER,?MODULE).

-define(ELEMENT,tc_element).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
    supervisor:start_child(?SERVER, [Value, LeaseTime]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpec = #{id => ?ELEMENT,
                  start => {?ELEMENT, start_link, []},
                  restart => temporary,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [?ELEMENT]},
    {ok, {SupFlags, [ChildSpec]}}.

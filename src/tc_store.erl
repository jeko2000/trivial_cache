-module(tc_store).

-export([init/0, insert/2, lookup/1, delete/1]).

-define(TABLE_NAME, ?MODULE).

init() ->
    ets:new(?TABLE_NAME, [public, named_table]),
    ok.

insert(Key, Pid) ->
    ets:insert(?TABLE_NAME, {Key, Pid}).

lookup(Key) ->
    case ets:lookup(?TABLE_NAME, Key) of
        [{_Key, Pid}] -> {ok, Pid};
        []           -> {error, not_found}
    end.

delete(Pid) ->
    ets:match_delete(?TABLE_NAME, {'_', Pid}).

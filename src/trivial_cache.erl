-module(trivial_cache).
-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tc_element:replace(Pid, Value);
        {error, not_found} ->
            {ok, Pid} = tc_element:create(Value),
            tc_store:insert(Key, Pid)
    end.

lookup(Key) ->
    try
        {ok, Pid} = tc_store:lookup(Key),
        {ok, Value} = tc_element:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            {error, not_found}
    end.

delete(Key) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tc_element:delete(Pid);
        {error, _Reason} ->
            ok
    end.

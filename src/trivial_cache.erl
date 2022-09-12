-module(trivial_cache).

-export([insert/3, insert/2, lookup/2, lookup/1, delete/1]).

-define(DEFAULT_LEASE_TIME, 60 * 60 * 24).

insert(Key, Value, LeaseTime) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tc_element:replace(Pid, Value);
        {error, not_found} ->
            {ok, Pid} = tc_element:create(Value, LeaseTime),
            tc_store:insert(Key, Pid)
    end.

insert(Key, Value) ->
    insert(Key, Value, ?DEFAULT_LEASE_TIME).

lookup(Key, DefaultValue) ->
    try
        {ok, Pid} = tc_store:lookup(Key),
        {ok, Value} = tc_element:fetch(Pid),
        {ok, Value}
    catch
        _Class:_Exception ->
            DefaultValue
    end.

lookup(Key) ->
    lookup(Key, {error, not_found}).


delete(Key) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tc_element:delete(Pid);
        {error, _Reason} ->
            ok
    end.

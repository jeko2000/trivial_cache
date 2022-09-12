-module(tc_element).

-behaviour(gen_server).

-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_LEASE_TIME, 60 * 60 * 24).

-record(state, {value, lease_time, start_time}).

start_link(Value, LeaseTime) ->
    gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
    tc_sup:start_child(Value, LeaseTime).

create(Value) ->
    create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

%% callbacks

init([Value, LeaseTime]) ->
    StartTime = current_time(),
    State = #state{value = Value,
                   lease_time = LeaseTime,
                   start_time = StartTime},
    Timeout = compute_timeout(StartTime, LeaseTime),
    {ok, State, Timeout}.

handle_call(fetch, _From, State) ->
    #state{value = Value,
           lease_time = LeaseTime,
           start_time = StartTime} = State,
    Timeout = compute_timeout(StartTime, LeaseTime),
    {reply, {ok, Value}, State, Timeout}.

handle_cast({replace, Value}, State) ->
    NewState = State#state{value = Value},
    Timeout = compute_timeout(State),
    {noreply, NewState, Timeout};

handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

%% private
current_time() ->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(Now).

compute_timeout(State) ->
    #state{lease_time = LeaseTime,
           start_time = StartTime} = State,
    compute_timeout(StartTime, LeaseTime).

compute_timeout(_StartTime, infinity) ->
    infinity;
compute_timeout(StartTime, LeaseTime) ->
    TimeElapsed = current_time() - StartTime,
    Difference = LeaseTime - TimeElapsed,
    case Difference of
        Time when Time =< 0 -> 0;
        Time                -> Time * 1000
    end.

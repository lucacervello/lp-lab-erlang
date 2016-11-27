-module(distributive_associative_store).

%% API exports
-export([start/1, store/2, lookup/1, stop/0]).

-import(master, [init/1]).

%%====================================================================
%% API functions
%%====================================================================

start(N) ->
    register(master, spawn(master, init, [N])).

store(K, V) ->
    rpc(whereis(master), {store, K, V}).

lookup(K) ->
    rpc(whereis(master), {lookup, K}).

stop() ->
    rpc(whereis(master), {exit}).

%%====================================================================
%% Internal functions
%%====================================================================


rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive
        {Pid, Reply} ->
            Reply
    end.

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_store_test() ->
    start(3),
    ?assertEqual(ok, store(a, 1)),
    stop().

simple_lookup_test() ->
    start(3),
    store(a, 1),
    ?assertEqual([1], lookup(a)).

-endif.

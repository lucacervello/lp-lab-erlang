-module(slave).

-export([init/0]).

init() ->
    receive
        {From, {store, K, V}} ->
            Kvs = case get(state) of
                      undefined -> #{};
                      _  -> get(state)
                  end,
            NewKvs = case maps:find(K, Kvs) of
                         {ok, Value} -> maps:update(K, [V | Value], Kvs);
                         error -> maps:put(K, [V], Kvs)
                     end,
            put(state, NewKvs),
            From ! {self(), ok},
            init();
        {From, {lookup, K}} ->
            V = case maps:find(K, get(state)) of
                    {ok, Value} -> Value;
                    error -> exit(self(), "Key not exists")
                end,
            From ! {self(), V},
            init();
        {From, {get_all}} ->
            From ! {self(), get(state)},
            init();
        {From, {update_all, NewState}} ->
            put(state, NewState),
            From ! {self(), ok},
            init()
    end.

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_store_test() ->
    Pid = spawn(fun () -> init() end),
    Pid ! {self(), {store, a, 1}},
    receive
        X -> ?assertEqual({Pid, ok}, X)
    end.

simple_lookup_test() ->
    Pid = spawn(fun () -> init() end),
    Pid ! {self(), {store, a, 1}},
    Pid ! {self(), {lookup, a}},
    receive
        {Pid, ok} -> ok;
         X -> ?assertEqual({Pid, 1}, X)
    end.

-endif.

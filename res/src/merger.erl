-module(merger).

-export([start/0]).

-import(reverser, [reverser_loop/0]).

start() ->
    register(server, spawn(fun () -> loop() end)),
    true.

loop() ->
    receive
        {From, reverse, Str, N} ->
            Str_splitted = split(Str, N),
            Nodes = [spawn(reverser, reverser_loop, []) || _ <- lists:seq(1, length(Str_splitted))],
            io:format("I got ~p and I'll ask ~p actors to reverse it! ~n", [Str, length(Nodes)]),
            ReversedList = [ rpc(X, {reverse, R}) || {X, R} <- lists:zip(Nodes, Str_splitted)],
            ReversedString = lists:foldl(fun (X, Acc) -> X ++ Acc end, [], ReversedList),
            From ! {{server, node()}, reversed, ReversedString},
            lists:foreach(fun(X) -> exit(X, normal) end, Nodes),
            loop();
        {From, close} ->
            io:format("I'm closing ...~n"),
            From ! {{server, node()}, closed},
            exit(self(), normal)
    end.


rpc(From, Q) ->
    From ! {self(), Q},
    receive
        {From, Reply} ->
            Reply
    end.

split(S, N) when N >= length(S) ->
    [S];
split(S, N) ->
    {S1, S2} = lists:split(N, S),
    [S1 | split(S2, N)].

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assertEqual(["ciao", "ciao"], split("ciaociao", 4)).

-endif.

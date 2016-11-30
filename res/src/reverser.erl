-module(reverser).

-export([reverser_loop/0]).

reverser_loop() ->
    receive
        {From, {reverse, Str}} ->
            From ! {self(), lists:reverse(Str)}
    end.

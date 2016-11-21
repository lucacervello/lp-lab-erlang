-module(list_comprehensions).

%% API exports
-export([squared_int/1, intersect/2, symmetric_difference/2]).

%%====================================================================
%% API functions
%%====================================================================

squared_int(Ls) ->
    [X * X || X <- Ls, is_integer(X)].

intersect(Ls1, Ls2) ->
    [X || X <- Ls1, Y <- Ls2, X =:= Y].

symmetric_difference(Ls1, Ls2) ->
    [X || X <- Ls1, not(lists:member(X, Ls2))] ++ [Y || Y <- Ls2, not(lists:member(Y, Ls1))].
    

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Test
%%====================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% simple test given by the professor

squared_int_test() ->
    ?assertEqual([1, 10000, 81], squared_int([1, hello, 100, boo, "boo", 9])).

intersect_test() ->
    ?assertEqual([4,5], intersect([1, 2, 3, 4, 5], [4, 5, 6, 7, 8])).

symmetric_difference_test() ->
    ?assertEqual([1, 2, 3, 6, 7, 8], symmetric_difference([1, 2, 3, 4, 5], [4, 5, 6, 7, 8])).

-endif.

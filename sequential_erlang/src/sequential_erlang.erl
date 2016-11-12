-module(sequential_erlang).

%% API exports
-export([is_palindrome/1, is_an_anagram/2, factors/1, is_proper/1]).

%%====================================================================
%% API functions
%%====================================================================
-spec is_palindrome(string()) -> boolean().
is_palindrome(Str) ->
    clean(Str) == lists:reverse(clean(Str)).

-spec is_an_anagram(string(), list(string())) -> boolean().
is_an_anagram(Str, Lst) ->
    SortedStr = lists:sort(Str),
    lists:any(fun (X) -> lists:sort(X) == SortedStr end, Lst).

-spec factors(integer()) -> list(integer()).
factors(N) ->
    lists:filter(fun (X) -> N rem X == 0 andalso is_prime(X) end, lists:seq(1, N)).

-spec is_proper(integer()) -> boolean().
is_proper(N) ->
    FN = factors(N),
    lists:sum(FN) == lists:foldl(fun (X, Acc) -> X * Acc end, 1, FN).

%%====================================================================
%% Internal functions
%%====================================================================

clean(Str) ->
    lists:filter(fun (X) -> lists:member(X, lists:seq($a, $z)) end, string:to_lower(Str)).

is_prime(1) ->
    true;
is_prime(2) ->
    true;
is_prime(N) ->
    length(lists:filter(fun (X) -> N rem X == 0 end, lists:seq(1, N))) == 2.

%% Test
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Palindrome test

palindrome_simple_test() ->
    ?assert(is_palindrome("anna")).

palindrome_not_so_simple_test() ->
    ?assert(is_palindrome("detartrated")).

palindrome_long_test() ->
    ?assert(is_palindrome("Do geese see God?")).

palindrome_another_long_test() ->
    ?assert(is_palindrome("Rise to vote, sir.")).


%% Anagram test

no_matches_test() ->
  ?assertEqual(false, is_an_anagram("diaper", ["hello", "world", "zombies", "pants"])).

detect_simple_anagram_test() ->
  ?assert(is_an_anagram("ant", ["tan", "stand", "at"])).

does_not_confuse_different_duplicates_test() ->
  ?assertEqual(false, is_an_anagram("galea", ["eagle"])).

eliminate_anagram_subsets_test() ->
  ?assertEqual(false, is_an_anagram("good", ["dog", "goody"])).

detect_anagram_test() ->
  ?assert(is_an_anagram("listen", ["enlists", "google", "inlets", "banana"])).

multiple_anagrams_test() ->
  ?assert(is_an_anagram("allergy", ["gallery", "ballerina", "regally", "clergy", "largely", "leading"])).

simple_test() ->
    ?assert(is_prime(5)).

%% Factors test

simple_factors_test() ->
    ?assertEqual([1,2,3], factors(6)).

another_simple_factors_test() ->
    ?assertEqual([1,3], factors(9)).

%% Proper test

simple_proper_test() ->
    ?assert(is_proper(6)).

other_proper_test() ->
    ?assertEqual(false, is_proper(9)).

-endif.

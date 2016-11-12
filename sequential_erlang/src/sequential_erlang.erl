-module(sequential_erlang).

%% API exports
-export([is_palindrome/1, is_an_anagram/2, is_proper/1]).

%%====================================================================
%% API functions
%%====================================================================
-spec is_palindrome(string) -> bool.
is_palindrome(Str) ->
    S = string:to_lower(Str),
    Sr = lists:reverse(string:to_lower(Str)),
    S == Sr.

is_an_anagram(Str, Lst) ->
    true.

is_proper(N) ->
    true.

%%====================================================================
%% Internal functions
%%====================================================================


%% Test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    fun () -> ?assert(1 + 1 =:= 2) end.

anagram_simple_test() ->
    ?assert(is_palindrome("anna")).

-endif.

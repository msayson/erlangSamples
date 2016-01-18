% Test module for recursiveFns.erl
%
% To run tests:
% - Compile recursiveFns and recursiveFns_test modules
% - In Erlang shell, run command:
%   > recursiveFns_test:test()
%
% Credits:
% 1) Modified assert_fail/4 from a StackOverflow post on Erlang error testing
%    http://stackoverflow.com/questions/33722462/how-to-test-errors-handling-in-common-test

-module(recursiveFns_test).

-import(recursiveFns, [nthtail/2, prefix/2, search/2]).

-include_lib("eunit/include/eunit.hrl").

assert_fail(Fun, Args, ExceptionType, ExceptionValue) ->
   try apply(Fun, Args) of
      _ -> ct:fail()
   catch
      ExceptionType:ExceptionValue -> ok
   end.

nthtail_test() ->
   [  ?assertEqual([], nthtail(2, [])),
      ?assertEqual([], nthtail(2, [1,2])),
      ?assertEqual([5], nthtail(0, [5])),
      ?assertEqual([3,4], nthtail(2, [1,2,3,4])),
      ?assertEqual("llo", nthtail(2, "hello")),
      ?assert(assert_fail(fun recursiveFns:nthtail/2, [-2, [2, 3]], error, function_clause) =:= ok),
      ?assert(assert_fail(fun recursiveFns:nthtail/2, [2, 3], error, function_clause) =:= ok),
      ?assert(assert_fail(fun recursiveFns:nthtail/2, [[1], [5]], error, function_clause) =:= ok)
   ].

prefix_test() ->
   [  ?assert(prefix([], [1,2,3])),
      ?assert(prefix([1,2,3], [1,2,3])),
      ?assert(prefix("he", "hello")),
      ?assert(prefix("a", "hello") =:= false),
      ?assert(prefix("help", "hello") =:= false),
      ?assert(assert_fail(fun recursiveFns:prefix/2, [1, [1]], error, function_clause) =:= ok),
      ?assert(assert_fail(fun recursiveFns:prefix/2, [[1], 1], error, function_clause) =:= ok)
   ].

search_test() ->
   [  ?assert(assert_fail(fun recursiveFns:search/2, [1, [1]], error, function_clause) =:= ok),
      ?assert(assert_fail(fun recursiveFns:search/2, [[1], 1], error, function_clause) =:= ok),
      ?assertEqual([], search([1,2], [])),
      ?assertEqual([], search([1,2,3], [1,2])),
      ?assertEqual([1], search([], [])), % [] is a prefix of []
      ?assertEqual([1,2,3], search([], "hi")),
      ?assertEqual([], search([1,2,3], [1,2])),
      ?assertEqual([1], search([1,2], [1,2,3,4])),
      ?assertEqual([2,4], search("an", "banana"))
   ].
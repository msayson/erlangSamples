-module(countInstances_test).

-import(countInstances, [runLengthEncode/1, addToCounts/2, addToConsecutiveCounts/2]).

-include_lib("eunit/include/eunit.hrl").

runLengthEncode_test() ->
   [  ?assertEqual([], runLengthEncode([])),
      ?assertEqual([{a, 1}], runLengthEncode([a])),
      ?assertEqual([{a, 1}, {b, 1}, {c, 1}, {b, 2}], runLengthEncode([a, b, c, b, b])),
      ?assertEqual([{5, 2}, {1, 1}, {-7, 1}, {1, 1}], runLengthEncode([5, 5, 1, -7, 1]))
   ].

addToCounts_test() ->
   [  ?assertEqual([{a, 1}], addToCounts(a, [])),
      ?assertEqual([{a, 6}], addToCounts(a, [{a, 5}])),
      ?assertEqual([{b, 3}, {a, 1}], addToCounts(a, [{b, 3}])),
      ?assertEqual([{a, 2}, {b, 5}, {c, 3}, {d, 2}], addToCounts(c, [{a, 2}, {b, 5}, {c, 2}, {d, 2}]))
   ].

addToConsecutiveCounts_test() ->
   [  ?assertEqual([{a, 1}], addToConsecutiveCounts(a, [])),
      ?assertEqual([{a, 6}], addToConsecutiveCounts(a, [{a, 5}])),
      ?assertEqual([{b, 3}, {a, 1}], addToConsecutiveCounts(a, [{b, 3}])),
      ?assertEqual([{a, 2}, {b, 5}, {a, 1}], addToConsecutiveCounts(a, [{a, 2}, {b, 5}]))
   ].

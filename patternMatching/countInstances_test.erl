-module(countInstances_test).

-import(countInstances, [
      runLengthEncode/1, runLengthEncodeWithPos/1, longestRunOfV/2, appendRunLengthEncodingWithPos/2,
      addToCounts/2, addToConsecutiveCounts/2, addToConsecCountsWithPos/3
   ]).

-include_lib("eunit/include/eunit.hrl").

runLengthEncode_test() ->
   [  ?assertEqual([], runLengthEncode([])),
      ?assertEqual([{a, 1}], runLengthEncode([a])),
      ?assertEqual([{a, 1}, {b, 1}, {c, 1}, {b, 2}], runLengthEncode([a, b, c, b, b])),
      ?assertEqual([{5, 2}, {1, 1}, {-7, 1}, {1, 1}], runLengthEncode([5, 5, 1, -7, 1]))
   ].

runLengthEncodeWithPos_test() ->
   [  ?assertEqual([], runLengthEncodeWithPos([])),
      ?assertEqual([{a,3,0}], runLengthEncodeWithPos([a,a,a])),
      ?assertEqual([{a,2,0}, {b,3,2}, {a,1,5}], runLengthEncodeWithPos([a,a,b,b,b,a]))
   ].

longestRunOfV_test() ->
   [  ?assertEqual({0,0}, longestRunOfV([], a)),
      ?assertEqual({0,0}, longestRunOfV([{a,1,0},{b,2,1}], z)),
      ?assertEqual({2,0}, longestRunOfV([{a,2,0}], a)),
      ?assertEqual({3,4}, longestRunOfV([{a,1,0},{b,3,1},{a,3,4},{b,1,7}], a))
   ].

appendRunLengthEncodingWithPos_test() ->
   [  ?assertEqual([], appendRunLengthEncodingWithPos([], [])),
      ?assertEqual([{a,3,4}, {b,5,7}], appendRunLengthEncodingWithPos([{a,3,4}, {b,5,7}], [])),
      ?assertEqual([{a,5,0},{b,6,5}], appendRunLengthEncodingWithPos([{a,5,0},{b,2,5}], [{b,4,7}])),
      ?assertEqual([{a,5,0},{b,4,5}], appendRunLengthEncodingWithPos([{a,2,0}], [{a,3,0},{b,4,3}])),
      ?assertEqual([{a,3,0},{b,1,3},{c,2,4},{a,2,6}], appendRunLengthEncodingWithPos([{a,3,0},{b,1,3}], [{c,2,0},{a,2,2}]))
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

addToConsecCountsWithPos_test() ->
   [  ?assertEqual([{a, 1, 0}], addToConsecCountsWithPos(a, [], 0)),
      ?assertEqual([{a, 6, 3}], addToConsecCountsWithPos(a, [{a, 5, 3}], 0)),
      ?assertEqual([{a, 2, 4}, {b, 1, 6}], addToConsecCountsWithPos(b, [{a, 2, 4}], 6)),
      ?assertEqual([{a, 2, 0}, {b, 1, 2}, {a, 1, 123}], addToConsecCountsWithPos(a, [{a, 2, 0}, {b, 1, 2}], 123))
   ].

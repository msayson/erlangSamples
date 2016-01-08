% Test module for mergeSort.erl
%
% To run tests:
% - Compile mergeSort and mergeSort_test modules
% - In Erlang shell, run command:
%   > mergeSort_test:test()

-module(mergeSort_test).

-import(mergeSort, [mergeSort/1, merge/2, split/1]).

-include_lib("eunit/include/eunit.hrl").

split_test() ->
  [ ?assertEqual({[],[]}, split([])),
    ?assertEqual({[],[5]}, split([5])),
    ?assertEqual({[5],[6,7]}, split([5,6,7])),
    ?assertEqual({[5,6],[7,8]}, split([5,6,7,8]))].

merge_test() ->
  [ ?assertEqual([], merge([], [])),
    ?assertEqual([1,2], merge([1,2],[])),
    ?assertEqual([1,2], merge([],[1,2])),
    ?assertEqual([1,2,3,4,7,8,9], merge([2,4,7],[1,3,8,9])),
    ?assertEqual([1,2,3,4,7,8,9], merge([1,3,8,9],[2,4,7]))].

mergeSort_test() ->
  [ ?assertEqual([], mergeSort([])),
    ?assertEqual([1], mergeSort([1])),
    ?assertEqual([1,2], mergeSort([2,1])),
    ?assertEqual([1,2,3,4], mergeSort([4,3,2,1])),
    ?assertEqual([1,2,3,4,5], mergeSort([2,5,1,4,3]))].

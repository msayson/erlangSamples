-module(listOfList_test).

-import(listOfList, [
      is_lol/1, is_rect/1, transpose/1
   ]).

-include_lib("eunit/include/eunit.hrl").

is_lol_test() ->
   [  ?assert(is_lol([])),
      ?assert(is_lol([[]])),
      ?assert(is_lol([[],[],[]])),
      ?assert(is_lol([[a,b,c],[[1],[],a],[],[a]])),
      ?assert(is_lol(1) =:= false),
      ?assert(is_lol([1]) =:= false),
      ?assert(is_lol([[a],b,c]) =:= false)
   ].

is_rect_test() ->
   [  ?assert(is_rect([])),
      ?assert(is_rect([[]])),
      ?assert(is_rect([[],[],[]])),
      ?assert(is_rect([[a,b,c],[1,2,3],[[],1,a]])),
      ?assert(is_rect([[a],[1,2,3]]) =:= false),
      ?assert(is_rect([[a],[]]) =:= false),
      ?assert(is_rect(a) =:= false),
      ?assert(is_rect([1,2]) =:= false),
      ?assert(is_rect([[a],1]) =:= false)
   ].

transpose_test() ->
   [  ?assertEqual([], transpose([])),
      ?assertEqual([], transpose([[],[],[]])),
      ?assertEqual([[1,2,3]], transpose([[1],[2],[3]])),
      ?assertEqual([[a,b,c],[1,2,3]], transpose([[a,1],[b,2],[c,3]])),
      ?assertError(function_clause, transpose(1)),
      ?assertError(function_clause, transpose([[a],[]])),
      ?assertError(function_clause, transpose([[],[a,a]])),
      ?assertError(function_clause, transpose([[a,b],[c,d,e]]))
   ].

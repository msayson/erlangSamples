-module(mergeSort).
-export([mergeSort/1, merge/2, split/1]).

% split list into a tuple of two sublists of roughly equal length
% if list has odd number of elements, puts one more element into right list
split([]) -> {[], []}; % ";" indicates there exist more cases to try if this doesn't match input
split([A]) -> {[], [A]};
split(L) ->
   LeftLen = length(L) div 2,
   {
      lists:sublist(L, 1, LeftLen),
      lists:sublist(L, LeftLen+1, LeftLen+1) %truncates if passes end of L
   }.

% merges two sorted lists into a single sorted list
merge(L, []) -> L;
merge([], R) -> R;
merge([H1|T1], [H2|T2]) ->
   if
      H1 < H2 -> [H1 | merge(T1, [H2|T2])];
      true    -> [H2 | merge([H1|T1], T2)]
   end.

% sorts list using merge sort algorithm
mergeSort([]) -> [];
mergeSort([A]) -> [A];
mergeSort(List) ->
   {L, R} = split(List),
   LSorted = mergeSort(L),
   RSorted = mergeSort(R),
   merge(LSorted, RSorted).

-module(listOfList).

-export [is_lol/1, is_rect/1, transpose/1].

% is_lol(L) returns true iff L is a list of lists
is_lol([]) -> true;
is_lol([H|T]) ->
   if
      is_list(H) -> is_lol(T);
      true       -> false
   end;
is_lol(_) -> false.

% is_rect(L) returns true iff L is a list of lists
% such that all sublists have equal length
is_rect([]) -> true;
is_rect([H|T]) ->
   if
      is_list(H) -> is_rect(T, length(H));
      true -> false
   end;
is_rect(_) -> false.
is_rect([], _) -> true;
is_rect([H|T], Len) ->
   if
      length(H) =:= Len -> is_rect(T, Len);
      true -> false
   end.

% transpose(X) produces the list Y such that
% the nth element of Y is the list of the nth elements of X's sublists.
transpose(L) ->
   IsRect = is_rect(L),
   if
      IsRect -> transposeRect(L);
      true -> error(function_clause)
   end.

transposeRect([]) -> [];
transposeRect([[]|_]) -> [];
transposeRect([H|T]) ->
   transpose([H|T], length(H), 1).

transpose([], _, _) -> [];
transpose(_, LenSublist, CurrPos) when CurrPos > LenSublist -> [];
transpose(ListOfLists, LenSublist, CurrPos) ->
   [nthOfEach(ListOfLists, CurrPos) | transpose(ListOfLists, LenSublist, CurrPos + 1)].

nthOfEach([], _) -> [];
nthOfEach([H|T], N) ->
   [lists:nth(N, H)|nthOfEach(T, N)].

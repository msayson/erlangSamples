-module(listFns).
-export([nthtail/2, prefix/2, search/2]).

% Returns nth tail of a list
% Requires N to be a non-negative integer and L to be a list
nthtail(N, L)
   when (is_integer(N) and is_list(L)) ->
      nthtail_helper(N, L).

nthtail_helper(0, L) -> L;
nthtail_helper(N, _) when (N < 0) -> erlang:error(function_clause); % N must be non-negative
nthtail_helper(_, []) -> [];
nthtail_helper(N, [_|T]) -> nthtail_helper(N-1, T).

% prefix(List1, List2) returns true iff List1 is a prefix of List2
% Requires List1 and List2 to be lists
prefix(List1, List2)
   when (is_list(List1) and is_list(List2)) ->
      prefix_helper(List1, List2).

prefix_helper([], _) -> true;
prefix_helper([H|T1], [H|T2]) -> prefix_helper(T1, T2);
prefix_helper(_, _) -> false.

% search(List1, List2) returns a list of indices such that
%   List1 is a prefix of List2 starting from each listed index of List2
% Requires List1 and List2 to be lists
%
% eg. search("he", "hello") -> [1]
%     search([1,2], [1,2,1,2,3]) -> [1,3]
%     search([1,2], [1]) -> []
%     search([], []) -> [1] % consider [] to be a prefix of []
search(List1, List2)
   when (is_list(List1) and is_list(List2)) ->
      search_helper(List1, List2, 1, []).

search_helper([], [], Pos, Indices) -> Indices ++ [Pos]; % [] is a prefix of []
search_helper([], [_|T], Pos, Indices) ->
   search_helper([], T, Pos + 1, Indices ++ [Pos]);
search_helper(_, [], _, Indices) -> Indices;
search_helper(L1, L2, Pos, Indices) ->
   case prefix(L1, L2) of
      true -> search_helper(L1, tl(L2), Pos + 1, Indices ++ [Pos]);
      false -> search_helper(L1, tl(L2), Pos + 1, Indices)
   end.

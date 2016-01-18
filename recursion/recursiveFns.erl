-module(recursiveFns).
-export([nthtail/2, prefix/2]).

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
prefix(List1, List2)
   when (is_list(List1) and is_list(List2)) ->
      prefix_helper(List1, List2).

prefix_helper([], _) -> true;
prefix_helper([H|T1], [H|T2]) -> prefix_helper(T1, T2);
prefix_helper(_, _) -> false.

-module(recursiveFns).
-export([nthtail/2]).

nthtail(N, L)
   when (is_integer(N) and is_list(L)) ->
      nthtail_helper(N, L).

nthtail_helper(0, L) -> L;
nthtail_helper(N, _) when (N < 0) -> erlang:error(function_clause); % N must be non-negative
nthtail_helper(_, []) -> [];
nthtail_helper(N, [_|T]) -> nthtail_helper(N-1, T).

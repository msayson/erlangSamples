-module(countInstances).

-export [runLengthEncode/1, addToCounts/2, addToConsecutiveCounts/2].

% Encodes elements of list as a compressed list of tuples
% {Item, Count} representing the number of consecutive
% appearances of each item in the list.
%
% Examples:
% runLengthEncode([a, a, a, b, b]) -> [{a, 3}, {b, 2}]
% runLengthEncode([a, a, b, c, c, a]) -> [{a, 2}, {b, 1}, {c, 2}, {a, 1}]
runLengthEncode(L) when is_list(L) ->
  rle(L, []).
rle([], Counts) -> Counts;
rle([H|T], Counts) ->
  rle(T, addToConsecutiveCounts(H, Counts)).

% Adds an item to a list encoding the total number of
% appearances of each item in the list.
%
% Examples:
% addToCounts(a, []) -> [{a, 1}]
% addToCounts(a, [{a, 2}]) -> [{a, 3}]
% addToCounts(a, [{a, 2}, {b, 1}]) -> [{a, 3}, {b, 1}]
addToCounts(Item, []) -> [{Item, 1}];
addToCounts(Item, [{Item, Count} | T]) ->
  [{Item, Count + 1} | T];
addToCounts(Item, [H|T]) -> [H|addToCounts(Item, T)].

% Adds an item to a list encoding the number of
% consecutive appearances of each item in the list.
%
% Examples:
% addToCounts(a, []) -> [{a, 1}]
% addToCounts(a, [{a, 2}]) -> [{a, 3}]
% addToCounts(a, [{a, 2}, {b, 1}]) -> [{a, 2}, {b, 1}, {a, 1}]
addToConsecutiveCounts(Item, []) -> [{Item, 1}];
addToConsecutiveCounts(Item, [{Item, Count}]) ->
  [{Item, Count + 1}];
addToConsecutiveCounts(Item, [H|T]) ->
  [H|addToConsecutiveCounts(Item, T)].

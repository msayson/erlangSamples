-module(countInstances).

-export [
      runLengthEncode/1, runLengthEncodeWithPos/1, longestRunOfV/2, appendRunLengthEncodingWithPos/2,
      addToCounts/2, addToConsecutiveCounts/2, addToConsecCountsWithPos/3
   ].

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

% Run-length encoding with start positions of each run
%
% Examples:
% runLengthEncodeWithPos([]) -> []
% runLengthEncodeWithPos([a,a,a]) -> [{a,3,0}]
% runLengthEncodeWithPos([1,2,2,2,3,3,1]) -> [{1,1,0},{2,3,1},{3,2,4},{1,1,6}]
runLengthEncodeWithPos(L) when is_list(L) ->
  rlep(L, [], 0).
rlep([], Counts, _) -> Counts;
rlep([H|T], Counts, Pos) ->
  rlep(T, addToConsecCountsWithPos(H, Counts, Pos), Pos+1).

% Returns longest run of Val in a run-length-position encoded list,
% in form {Length, Position}
%
% Examples:
% longestRunOfV([], a) -> {0,0}
% longestRunOfV([{a,2,0},{b,2,2},{a,1,4}], a) -> {2,0}
% longestRunOfV([{a,2,0},{b,2,2},{a,3,4}], a) -> {3,4}
longestRunOfV(L, Val) when is_list(L) ->
  longestRunOfV(L, Val, 0, 0).
longestRunOfV([], _, LenLongest, PosLongest) -> {LenLongest, PosLongest};
longestRunOfV([{Val,Length,Pos}|T], Val, LenLongestSoFar, PosLongestSoFar) ->
  {LenLongest, PosLongest} = if
    Length > LenLongestSoFar -> {Length, Pos};
    true                     -> {LenLongestSoFar, PosLongestSoFar}
  end,
  longestRunOfV(T, Val, LenLongest, PosLongest);
longestRunOfV([_|T], Val, LenLongestSoFar, PosLongestSoFar) ->
  longestRunOfV(T, Val, LenLongestSoFar, PosLongestSoFar).

% Appends two run-length encodings with positions
%
% Examples:
% appendRunLengthEncodingWithPos([{a,2,0}], [{a,4,2}]) -> [{a,6,0}]
% appendRunLengthEncodingWithPos([{a,2,0}], [{b,1,2}, {a,1,3}]) -> [{a,2,0},{b,1,2},{a,1,3}]
appendRunLengthEncodingWithPos([], L) -> L;
appendRunLengthEncodingWithPos(L, []) -> L;
appendRunLengthEncodingWithPos([{Item, Count1, Pos1}], [{Item, Count2, _}|T]) ->
  [{Item, Count1 + Count2, Pos1} | addIndexToRuns(T, Pos1)];
appendRunLengthEncodingWithPos([H], L2) -> [H|L2];
appendRunLengthEncodingWithPos([H|T], L2) -> [H|appendRunLengthEncodingWithPos(T, L2)].

addIndexToRuns([], _) -> [];
addIndexToRuns([{Item, Count, Pos}|T], Offset) ->
  [{Item, Count, Pos+Offset} | addIndexToRuns(T, Offset)].

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

% Adds an item to a list encoding the number of
% consecutive appearances of each item in the list
% and the starting position of each run.
%
% Examples:
% addToConsecCountsWithPos(a, [], 0) -> [{a, 1, 0}]
% addToConsecCountsWithPos(a, [{a, 1, 0}], 1) -> [{a, 2, 0}]
% addToConsecCountsWithPos(b, [{a, 2, 0}], 2) -> [{a, 2, 0}, {b, 1, 2}]
addToConsecCountsWithPos(Item, [], Pos) -> [{Item, 1, Pos}];
addToConsecCountsWithPos(Item, [{Item, Count, Pos}], _) ->
  [{Item, Count + 1, Pos}];
addToConsecCountsWithPos(Item, [H|T], Pos) ->
  [H|addToConsecCountsWithPos(Item, T, Pos)].

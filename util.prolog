:- module(util, [read_file_lines_to_strings/2,
                 read_file_lines_to_codes/2,
                 read_file_lines_to_codes/4,
                 read_file_lines_to_chars/2,
                 read_file_to_chars/2,
                 read_file_lines_to_chars/4,
                 read_file_lines_to_chars/5,
                 replace_nth/4,
                 indexed/2,
                 last_index/3,
                 mapcat/3,
                 mean/2,
                 variance/2,
                 bitset_set/4,
                 bitset_is_set/2,
                 bitset_chars/2,
                 bitset_string/2,
                 goal_bitset/3,
                 first_index/3]).

:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(apply_macros)). % I guess you need this for efficient YALL usage??
:- use_module(library(charsio), [with_output_to_chars/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [same_length/2, append/3, member/2]).
:- use_module(library(readutil), [read_line_to_string/2, read_line_to_codes/2, read_stream_to_codes/2]).
:- use_module(library(yall)).

read_stream_lines_to_strings(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_strings(Stream, [Line | MoreLines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_stream_lines_to_strings(Stream, MoreLines).

read_file_lines_to_strings(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_strings(Stream, Lines),
    close(Stream).

read_stream_lines_to_codes(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_codes(Stream, [Line | MoreLines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Line),
    read_stream_lines_to_codes(Stream, MoreLines).

read_file_lines_to_codes(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_codes(Stream, Lines),
    close(Stream).

read_stream_lines_to_chars(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_chars(Stream, [Chars | More]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    read_stream_lines_to_chars(Stream, More).

read_file_lines_to_chars(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_chars(Stream, Lines),
    close(Stream).

read_file_to_chars(Path, Chars) :-
    open(Path, read, Stream),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    maplist([Code, Char]>>char_code(Char, Code), Codes, Chars).

init_row(_Goal, [], _P, []).

init_row(Goal, [Cell | More], RowNum-ColNum, Out) :-
    (
        call(Goal, RowNum-ColNum, Cell, CellOut)
    ->  Out = [CellOut | MoreOut]
    ;   Out = MoreOut
    ),
    NextColNum #= ColNum + 1,
    init_row(Goal, More, RowNum-NextColNum, MoreOut).

init_rows(_Goal, [], _RowNum, []).

init_rows(Goal, [Row | More], RowNum, Out) :-
    init_row(Goal, Row, RowNum-0, Out0),
    NextRowNum #= RowNum + 1,
    append(Out0, Out1, Out),
    init_rows(Goal, More, NextRowNum, Out1).

read_file_lines_to_chars(Path, Size, Goal, Out) :-
    read_file_lines_to_chars(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Goal, Rows, 0, Out).

read_file_lines_to_codes(Path, Size, Goal, Out) :-
    read_file_lines_to_codes(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Goal, Rows, 0, Out).

replace_nth(Index, List, Element, NewList) :-
    same_length(List, NewList),
    length(Before, Index),
    append(Before, [_ | After], List),
    append(Before, [Element | After], NewList).

indexed([], _I, []).
indexed([X | More], I, [I-X | MoreIndexed]) :- NextI #= I + 1, indexed(More, NextI, MoreIndexed).

indexed(List, Out) :- indexed(List, 0, Out).

% first_index([a, a, b, c, b, c, d, b, e], [X]>>(X = 'b'), I).
% I = 2
% I #> 2, first_index([a, a, b, c, b, c, d, b, e], [X]>>(X = 'b'), I).
% I = 4

%!  last_index(List, Pred, I) is undefined.
%
%   You can use CLP(FD) constraints on I to skip values from consideration as an optimization.
last_index([H|T], Pred, I) :-
    (
        I #= NextI + 1,
        last_index(T, Pred, NextI)
    )
->  true
;   I #= 0,
    call(Pred, H).

% last_index([a, a, b, c, b, c, d, b, e], [X]>>(X = 'b'), I).
% I = 7
%
% I #< 7, last_index([a, a, b, c, b, c, d, b, e], [X]>>(X = 'b'), I).
% I = 4

mapcat(Pred, List, Out) :-
    maplist(Pred, List, Out0),
    foldl([L, Acc0, Acc1]>>append(Acc0, L, Acc1), Out0, [], Out).

init_row(_Goal, [], _P, Acc, Acc).

init_row(Goal, [Cell | More], RowNum-ColNum, Acc0, Out) :-
    (
        call(Goal, RowNum-ColNum, Cell, Acc0, Acc1)
    ->  true
    ;   Acc1 = Acc0
    ),
    NextColNum #= ColNum + 1,
    init_row(Goal, More, RowNum-NextColNum, Acc1, Out).

init_rows(_Goal, [], _RowNum, Out, Out).

init_rows(Goal, [Row | More], RowNum, Acc0, Out) :-
    init_row(Goal, Row, RowNum-0, Acc0, Acc1),
    NextRowNum #= RowNum + 1,
    init_rows(Goal, More, NextRowNum, Acc1, Out).

read_file_lines_to_chars(Path, Size, Goal, Init, Out) :-
    read_file_lines_to_chars(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Goal, Rows, 0, Init, Out).

%!  mean(List, Mean) is semidet.
%
%   Calculate the mean of a list of numbers.
mean(Numbers, Mean) :-
    aggregate_all(sum(X), (member(Numbers, X)), Sum),
    length(Numbers, Length),
    Mean is Sum / Length.

%! deviation_square(Mean, Number, Square) is semidet.
deviation_square(Mean, Number, Square) :-
    Square is (Number - Mean) ^ 2.

%!  variance(Numbers, Variance) is semidet.
%
%   Calculate the variance of a list of numbers.
variance(Numbers, Variance) :-
    mean(Numbers, Mean),
    aggregate_all(sum(Square),
                  (
                      member(X, Numbers),
                      deviation_square(Mean, X, Square)
                  ),
                  SumOfSquares
                 ),
    length(Numbers, Length),
    Length > 0,
    Variance is SumOfSquares / Length.

%!  bitset_set(++BitSet0, ++Index, ++Bit, -BitSet1) is semidet.
%
%   BitSet1 is BitSet0 with the bit at Index (from the right, i.e. the least-significant-bit is Index 0) set to Bit.
%
%   BitSet is just a regular integer.
bitset_set(N0, Index, Bit, N1) :-
    Bit in { 0, 1 },
    Existing is getbit(N0, Index),
    (
        Existing #= Bit
    ->  N1 #= N0
    ;   BitMask #= 1 << Index,
        N1 #= N0 xor BitMask
    ).

%!  bitset_is_set(++BitSet, ++Index) is semidet.
%!  bitset_is_set(++BitSet, -Index) is nondet.
%!  bitset_is_set(-BitSet, ++Index) is det.
%
%   If BitSet and Index are both ground, succeeds if the bit at Index (starting with the least significant bit being
%   Index = 0) is 1.
%
%   If BitSet is ground, but Index is not, succeeds once for each Index of a bit that is 1 (starting with Index = 0).
%
%   If BitSet is unground and Index is ground, returns a bitset with just that bit set.
%
%   BitSet is just a regular integer.

bitset_is_set(BitSet, Offset, Index) :-
    BitSet #> 0,
    (
        ( var(BitSet), ground(Index) )
    ->  bitset_set(0, Index, 1, BitSet)
    ;   (  ground(BitSet), ground(Index) )
    ->  1 is getbit(BitSet, Index)
    ;   (  ground(BitSet), var(Index) )
    ->  LSBIndex is lsb(BitSet),
        Index0 #= Offset + LSBIndex,
        (
            Index #= Index0
        ;   NextBitSet is BitSet >> ( LSBIndex + 1),
            NextIndex #= Index0 + 1,
            bitset_is_set(NextBitSet, NextIndex, Index)
        )
    ).

bitset_is_set(BitSet, Index) :- bitset_is_set(BitSet, 0, Index).

%!  bitset_chars(++BitSet, -Chars) is det.
%!  bitset_chars(-BitSet, ++Chars) is det.
%!  bitset_chars(++BitSet, ++Chars) is det.
%
%   Chars is a list of characters representing BitSet, e.g. "0b110".
bitset_chars(BitSet, Chars) :-
    ground(Chars)
->  term_string(BitSet, String),
    string_chars(String, Chars)
;   with_output_to_chars(format("0b~2r", [BitSet]), Chars, []).

%!  bitset_string(++BitSet, -String) is det.
%!  bitset_string(-BitSet, ++String) is det.
%!  bitset_string(++BitSet, ++String) is det.
%
%   String is a list of characters representing BitSet, e.g. "0b110".
bitset_string(BitSet, String) :- bitset_chars(BitSet, Chars), string_chars(String, Chars).

%!  goal_bitset(Goal, ++List, -BitSet) is det.
%
%   BitSet is an integer whose bits are set to 1 for the indecies of items in List that satisfy Goal(Item).

goal_bitset(_Goal, [], _Position, BitSet, BitSet).

goal_bitset(Goal, [X|More], Position, BitSet0, BitSet) :-
    (
        call(Goal, X)
    ->  bitset_set(BitSet0, Position, 1, BitSet1)
    ;   BitSet1 = BitSet0
    ),
    NextPosition #= Position + 1,
    goal_bitset(Goal, More, NextPosition, BitSet1, BitSet).

goal_bitset(List, Goal, BitSet) :- goal_bitset(List, Goal, 0, 0, BitSet).

%!  first_index(++List, Goal, Index) is semidet.
%
%   Index is the index of the first item in List that satisfies Goal(Item).
%
%   You can use CLP(FD) constraints on Index to skip values from consideration as an optimization.
%
%   ?- first_index([a, a, b, c, b, c, d, b, e], call(=(b)), I).
%   I = 2.
%
%   ?- I #> 2, first_index([a, a, b, c, b, c, d, b, e], call(=(b)), I).
%   I = 4.

first_index([], _Goal, _Index0, _Index) :- fail.

first_index([X|More], Goal, Index0, Index) :-
    (
        Index #= Index0,
        call(Goal, X)
    )
->  true
;   NextIndex #= Index0 + 1,
    first_index(More, Goal, NextIndex, Index).

first_index(List, Goal, Index) :- first_index(List, Goal, 0, Index).

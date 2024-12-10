:- module(util, [read_file_lines_to_strings/2,
                 read_file_lines_to_codes/2,
                 read_file_lines_to_codes/4,
                 read_file_lines_to_chars/2,
                 read_file_lines_to_chars/4,
                 replace_nth/4,
                 indexed/2,
                 first_index/3,
                 last_index/3]).

:- use_module(library(lists), [same_length/2, append/3, reverse/2]).
:- use_module(library(readutil), [read_line_to_string/2, read_line_to_codes/2]).

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

init_rows(Goal, Rows, Out) :- init_rows(Goal, Rows, 0, Out).

read_file_lines_to_chars(Path, Size, Goal, Out) :-
    read_file_lines_to_chars(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Goal, Rows, Out).

read_file_lines_to_codes(Path, Size, Goal, Out) :-
    read_file_lines_to_codes(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Goal, Rows, Out).

replace_nth(Index, List, Element, NewList) :-
    same_length(List, NewList),
    length(Before, Index),
    append(Before, [_ | After], List),
    append(Before, [Element | After], NewList).

indexed([], _I, []).
indexed([X | More], I, [I-X | MoreIndexed]) :- NextI #= I + 1, indexed(More, NextI, MoreIndexed).

indexed(List, Out) :- indexed(List, 0, Out).

%!  first_index(List, Pred, I) is undefined.
%
%   You can use CLP(FD) constraints on I to skip values from consideration as an optimization.
first_index([H|T], Pred, I) :-
    (
        I #= 0,
        call(Pred, H)
    )
->  true
;   I #= NextI + 1,
    first_index(T, Pred, NextI).

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

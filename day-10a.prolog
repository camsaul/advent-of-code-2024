:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), []).
:- use_module(library(solution_sequences), [distinct/1]).

:- use_module(util, [read_file_lines_to_chars/2, indexed/2]).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).

init_cell(ColNum-Char, Acc0, Acc1) :- number_string(Height, [Char]), put_dict(ColNum, Acc0, Height, Acc1).

init_row(RowNum-Row, Acc0, Acc1) :-
    indexed(Row, IndexedRow),
    foldl(init_cell, IndexedRow, _{}, RowDict),
    put_dict(RowNum, Acc0, RowDict, Acc1).

init_rows(Rows, Out) :- indexed(Rows, IndexedRows), foldl(init_row, IndexedRows, _{}, Out).

path(example, 'day-10-example.txt').
path(actual, 'day-10.txt').

init(Source, Size, Out) :-
    path(Source, Path),
    read_file_lines_to_chars(Path, Rows),
    length(Rows, NumRows),
    [Row|_] = Rows,
    length(Row, NumCols),
    Size = NumRows-NumCols,
    format("Size = ~w~n", [Size]),
    init_rows(Rows, Out).

% left or right
next_position(_NumRows-NumCols, Row-Col, Row-NextCol) :-
    (
        NextCol #= Col + 1,
        NextCol #< NumCols
    )
;   (
        NextCol #= Col - 1,
        NextCol #>= 0
    ).

% up or down
next_position(NumRows-_NumCols, Row-Col, NextRow-Col) :-
    (
        NextRow #= Row + 1,
        NextRow #< NumRows
    )
;   (
        NextRow #= Row - 1,
        NextRow #>= 0
    ).

cell_height(Board, Row-Col, Value) :- Value = (Board.Row).Col.

trail_ending_at(_Size, Board, Position, 0, [Position]) :- cell_height(Board, Position, 0).

trail_ending_at(Size, Board, Position, Height, [Position|NextTrail]) :-
    Height #> 0,
    cell_height(Board, Position, Height),
    next_position(Size, Position, NextPosition),
    NextHeight #= Height - 1,
    trail_ending_at(Size, Board, NextPosition, NextHeight, NextTrail).

trail_between_points(Size, Board, StartPosition, EndPosition) :-
    trail_ending_at(Size, Board, EndPosition, 9, Trail),
    lists:last(Trail, StartPosition).

trail(Size, Board, [StartPosition, EndPosition]) :- trail_between_points(Size, Board, StartPosition, EndPosition).

% solve(example, N).
% solve(actual, N).
solve(Source, N) :-
    init(Source, Size, Board),
    findall(Trail, distinct(trail(Size, Board, Trail)), Trails),
    length(Trails, N).

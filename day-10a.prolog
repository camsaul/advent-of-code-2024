:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).

:- use_module(util, [read_file_lines_to_chars/2, indexed/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(solution_sequences), [distinct/1]).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).

init_cell(ColNum-Char, Acc0, Acc1) :-
    number_string(Height, [Char]),
    put_dict(ColNum, Acc0, _{height: Height}, Acc1).

init_row(RowNum-Row, Acc0, Acc1) :-
    indexed(Row, IndexedRow),
    foldl(init_cell, IndexedRow, _{}, RowDict),
    put_dict(RowNum, Acc0, RowDict, Acc1).

init_rows(Rows, Out) :-
    indexed(Rows, IndexedRows),
    foldl(init_row, IndexedRows, _{}, Out).

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

position(NumRows-NumCols, Row-Col) :- Row #>= 0, Row #< NumRows, Col #>= 0, Col #< NumCols.

% left or right
next_position(Size, Row-Col, Row-NextCol) :-
    (
        NextCol #= Col + 1
    ;   NextCol #= Col - 1
    ),
    position(Size, Row-NextCol).

% up or down
next_position(Size, Row-Col, NextRow-Col) :-
    (
        NextRow #= Row + 1
    ;   NextRow #= Row - 1
    ),
    position(Size, NextRow-Col).

cell_height(Board, Row-Col, Value) :- Value = ((Board.Row).Col).height.

trail_ending_at(_Size, Board, Position, 0, [Position]) :- cell_height(Board, Position, 0).

trail_ending_at(Size, Board, Position, Height, Trail) :-
    Height #> 0,
    cell_height(Board, Position, Height),
    next_position(Size, Position, NextPosition),
    NextHeight #= Height - 1,
    trail_ending_at(Size, Board, NextPosition, NextHeight, NextTrail),
    append(NextTrail, [Position], Trail).

trail_between_points(Size, Board, StartPosition, EndPosition) :-
    trail_ending_at(Size, Board, EndPosition, 9, [StartPosition|_]).

trail(Size, Board, [StartPosition, EndPosition]) :-
    position(Size, EndPosition),
    trail_between_points(Size, Board, StartPosition, EndPosition).

trailheads(Size, Board, Trailheads) :- findall(Trailhead, distinct(trail(Size, Board, Trailhead)), Trailheads).

% solve(example, N).
% solve(actual, N).
solve(Source, N) :- init(Source, Size, Board), trailheads(Size, Board, Trailheads), length(Trailheads, N).

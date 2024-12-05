:- use_module(library(apply), [maplist/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, nth0/3]).
:- use_module(library(yall)).
:- use_module(library(readutil), [read_line_to_string/2]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

diagonal_solution("MAS").
diagonal_solution("SAM").

point(NumRows-NumCols, Row-Col) :- Row #>= 0, Row #=< NumRows - 1, Col #>= 0, Col #=< NumCols - 1.

neighbor_point(Row-Col, downleft, NextRow-NextCol)  :- NextRow #= Row + 1, NextCol #= Col - 1.
neighbor_point(Row-Col, downright, NextRow-NextCol) :- NextRow #= Row + 1, NextCol #= Col + 1.
neighbor_point(Row-Col, upleft, NextRow-NextCol)    :- NextRow #= Row - 1, NextCol #= Col - 1.
neighbor_point(Row-Col, upright, NextRow-NextCol)   :- NextRow #= Row - 1, NextCol #= Col + 1.

point_value(Board, RowNum-ColNum, Value) :-
    nth0(RowNum, Board, Row),
    nth0(ColNum, Row, Value).

neighbor_value(Board, Point, Direction, Value) :-
    neighbor_point(Point, Direction, NeighborPoint),
    point_value(Board, NeighborPoint, Value).

solution_at_point(Board, CenterPoint) :-
    point_value(Board, CenterPoint, Center),
    neighbor_value(Board, CenterPoint, upleft,    UpLeft),
    neighbor_value(Board, CenterPoint, downleft,  DownLeft),
    neighbor_value(Board, CenterPoint, upright,   UpRight),
    neighbor_value(Board, CenterPoint, downright, DownRight),
    diagonal_solution([UpLeft, Center, DownRight]),
    diagonal_solution([DownLeft, Center, UpRight]).

board_of_size(Board, NumRows-NumCols) :-
    length(Board, NumRows),
    maplist({NumCols}/[Row]>>length(Row, NumCols), Board).

solution(Board, BoardSize, Point) :-
    point(BoardSize, Point),
    solution_at_point(Board, Point).

board(Board, NumSolutions) :-
    board_of_size(Board, BoardSize),
    board(Board, BoardSize, NumSolutions).

board(Board, BoardSize, NumSolutions) :-
    board_of_size(Board, BoardSize),
    (
        findall([Point], solution(Board, BoardSize, Point), Solutions)
    ;   NumSolutions #= 0
    ),
    length(Solutions, NumSolutions).

read_lines_to_chars(Stream, []) :- at_end_of_stream(Stream).

read_lines_to_chars(Stream, [Chars | More]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    read_lines_to_chars(Stream, More).

read_file_lines_to_chars(Path, Lines) :-
    open(Path, read, Stream),
    read_lines_to_chars(Stream, Lines),
    close(Stream).

run(NumSolutions) :- read_file_lines_to_chars("day-04.txt", Board), board(Board, NumSolutions).

partition_on_newlines([], []).

partition_on_newlines(Chars, [Row | MoreBoard]) :-
    append(Row, ['\n' | More], Chars)
    -> partition_on_newlines(More, MoreBoard)
    ;  (
           Row = Chars,
           MoreBoard = []
       ).

example(
"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX").

run_example(NumSolutions) :- example(Example), partition_on_newlines(Example, Board), board(Board, NumSolutions).

debug_solutions(Solution) :-
    example(Example),
    partition_on_newlines(Example, Board),
    board_of_size(Board, BoardSize),
    solution(Board, BoardSize, Solution).

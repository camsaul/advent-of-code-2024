%
% Alternate version of 4b that can also run backwards to generate boards.
%

:- use_module(library(apply), [maplist/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, nth0/3, member/2]).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(yall)).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

center_point(NumRows-NumCols, Row-Col) :- Row #>= 1, Row #=< NumRows - 2, Col #>= 1, Col #=< NumCols - 2.

neighbor_point(Row-Col, downleft, NextRow-NextCol)  :- NextRow #= Row + 1, NextCol #= Col - 1.
neighbor_point(Row-Col, downright, NextRow-NextCol) :- NextRow #= Row + 1, NextCol #= Col + 1.
neighbor_point(Row-Col, upleft, NextRow-NextCol)    :- NextRow #= Row - 1, NextCol #= Col - 1.
neighbor_point(Row-Col, upright, NextRow-NextCol)   :- NextRow #= Row - 1, NextCol #= Col + 1.

diagonal_solution([A, B, C]) :-
    [A, C] ins { 0'M, 0'S },
    B #= 0'A,
    (
        (A #= 0'M, C #= 0'S)
    ;   (A #= 0'S, C #= 0'M)
    ).

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

row_in_domain(Row) :- Row ins { 0'X, 0'M, 0'A, 0'S }.

board_of_size(Board, NumRows-NumCols) :-
    length(Board, NumRows),
    maplist({NumCols}/[Row]>>(length(Row, NumCols), row_in_domain(Row)), Board).

solution(Board, BoardSize, Point) :-
    center_point(BoardSize, Point),
    solution_at_point(Board, Point).

board(Board, NumSolutions) :-
    board_of_size(Board, BoardSize),
    board(Board, BoardSize, NumSolutions).

solutions(_Board, _BoardSize, 0, []) :- !.

solutions(Board, BoardSize, NumSolutions, [Point | More]) :-
    NewNumSolutions #= NumSolutions - 1,
    solutions(Board, BoardSize, NewNumSolutions, More),
    center_point(BoardSize, Point),
    Row-Col = Point,
    label([Row, Col]),
    \+ member(Point, More), % no duplicate solution points.
    solution(Board, BoardSize, Point).

total_num_solutions(Board, BoardSize, NumSolutions) :-
    findall([Point], solution(Board, BoardSize, Point), Solutions)
->  length(Solutions, NumSolutions)
;   NumSolutions #= 0.

board(Board, BoardSize, NumSolutions) :-
    board_of_size(Board, BoardSize),
    (
        var(NumSolutions)
    % just solving for number of solutions is somewhat easier than generating a new board. If I knew what I was doing
    % better I could probably figure out how to make the solutions() relation work both ways.
    ->  total_num_solutions(Board, BoardSize, NumSolutions)
    ;   (
            solutions(Board, BoardSize, NumSolutions, _Solutions),
            maplist(label, Board),
            total_num_solutions(Board, BoardSize, NumSolutions)
        )
    ).

read_lines_to_codes(Stream, []) :- at_end_of_stream(Stream).

read_lines_to_codes(Stream, [Chars | More]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    string_codes(String, Chars),
    read_lines_to_codes(Stream, More).

read_file_lines_to_codes(Path, Lines) :-
    open(Path, read, Stream),
    read_lines_to_codes(Stream, Lines),
    close(Stream).

run(NumSolutions) :- read_file_lines_to_codes("day-04.txt", Board), !, board(Board, NumSolutions).

write_board(Board) :- maplist([Row]>>(string_codes(S, Row), writeln(S)), Board).

partition_on_newlines([], []).

partition_on_newlines(Chars, [Row | MoreBoard]) :-
    append(Row, [0'\n | More], Chars)
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

example_board(Board) :-  example(Example), partition_on_newlines(Example, Board).

run_example(NumSolutions) :- example_board(Board), board(Board, NumSolutions).

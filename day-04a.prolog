:- use_module(library(apply), [maplist/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, nth0/3]).
:- use_module(library(yall)).
:- use_module(library(readutil), [read_line_to_string/2]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

solution("XMAS").
solution("SAMX").

direction(right).
direction(down).
direction(downright).
direction(downleft).

point(NumRows-NumCols, Row-Col) :-
    Row #>= 0,
    Row #=< NumRows - 1,
    Col #>= 0,
    Col #=< NumCols - 1.

next_point_in_direction(Row-Col, right, Row-NextCol)         :- NextCol #= Col + 1.
next_point_in_direction(Row-Col, down, NextRow-Col)          :- NextRow #= Row + 1.
next_point_in_direction(Row-Col, downleft, NextRow-NextCol)  :- NextRow #= Row + 1, NextCol #= Col - 1.
next_point_in_direction(Row-Col, downright, NextRow-NextCol) :- NextRow #= Row + 1, NextCol #= Col + 1.

point_value(Board, RowNum-ColNum, Value) :-
    nth0(RowNum, Board, Row),
    nth0(ColNum, Row, Value).

has_word_at_point(_Board, _Point, _Direction, []).

has_word_at_point(Board, Point, Direction, [Letter | MoreLetters]) :-
    point_value(Board, Point, Letter),
    direction(Direction),
    next_point_in_direction(Point, Direction, NextPoint),
    has_word_at_point(Board, NextPoint, Direction, MoreLetters).

solution_at_point(Board, Point, Direction) :-
    solution(Word),
    has_word_at_point(Board, Point, Direction, Word).

board_of_size(Board, NumRows-NumCols) :-
    length(Board, NumRows),
    maplist({NumCols}/[Row]>>length(Row, NumCols), Board).

solution(Board, BoardSize, Point, Direction) :-
    point(BoardSize, Point),
    solution_at_point(Board, Point, Direction).

board(Board, NumSolutions) :-
    board_of_size(Board, BoardSize),
    board(Board, BoardSize, NumSolutions).

board(Board, BoardSize, NumSolutions) :-
    board_of_size(Board, BoardSize),
    (
        findall([Point, Direction], solution(Board, BoardSize, Point, Direction), Solutions)
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

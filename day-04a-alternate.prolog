%
% This was an alternate attempt at solving Day 4A but also having the board() relation work backwards, i.e. have it be
% able to generate boards. The other solution worked only for smaller boards, but couldn't generate larger boards in a
% timely manner. So it does do that reasonably well, but the code is a lot hairier, and at some point I broke solving
% boards. Maybe I will revisit this in the future when I actually know what I'm doing.
%

:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, nth0/3]).
:- use_module(library(yall)).

:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

solution("XMAS").

direction(right). direction(down). direction(downright). direction(downleft).
direction(left).  direction(up).   direction(upright).   direction(upleft).

next_point_in_direction(Row-Col, right, Row-NextCol)         :- NextCol #= Col + 1.
next_point_in_direction(Row-Col, down, NextRow-Col)          :- NextRow #= Row + 1.
next_point_in_direction(Row-Col, downleft, NextRow-NextCol)  :- NextRow #= Row + 1, NextCol #= Col - 1.
next_point_in_direction(Row-Col, downright, NextRow-NextCol) :- NextRow #= Row + 1, NextCol #= Col + 1.
next_point_in_direction(Row-Col, left, Row-NextCol)          :- NextCol #= Col - 1.
next_point_in_direction(Row-Col, up, NextRow-Col)            :- NextRow #= Row - 1.
next_point_in_direction(Row-Col, upleft, NextRow-NextCol)    :- NextRow #= Row - 1, NextCol #= Col - 1.
next_point_in_direction(Row-Col, upright, NextRow-NextCol)   :- NextRow #= Row - 1, NextCol #= Col + 1.

point(NumBoard-NumCols, Row-Col) :-
    Row #>= 0,
    Row #=< NumBoard - 1,
    Col #>= 0,
    Col #=< NumCols - 1.

next_points_in_direction(_BoardSize, _Point, _Direction, 0, []) :- !.

next_points_in_direction(BoardSize, Point, Direction, NumPoints, [NextPoint | MorePoints]) :-
    point(BoardSize, Point),
    next_point_in_direction(Point, Direction, NextPoint),
    point(BoardSize, NextPoint),
    NewNumPoints #= NumPoints - 1,
    next_points_in_direction(BoardSize, NextPoint, Direction, NewNumPoints, MorePoints).

% word_path is a list of coordinates for a "word", a list of 4 logic vars that represent a place that might be XMAS.

word_path_at_point_in_direction(BoardSize, Point, Direction, [ Point | MorePoints ]) :-
    direction(Direction),
    next_points_in_direction(BoardSize, Point, Direction, 3, MorePoints).

word_path(BoardSize, Path) :-
    point(BoardSize, Row-Col),
    label([Row, Col]),
    word_path_at_point_in_direction(BoardSize, Row-Col, _Direction, Path),
    length(Path, 4).

point_value(Board, RowNum-ColNum, Value) :-
    nth0(RowNum, Board, Row),
    nth0(ColNum, Row, Value).

word_paths(BoardSize, Paths) :- findall(Path, word_path(BoardSize, Path), Paths).

word(Board, Path, Word) :- maplist(call(point_value(Board)), Path, Word).

words_at_paths(Board, Paths, Words) :- maplist(call(word(Board)), Paths, Words).

words(Board, BoardSize, Words) :-
    word_paths(BoardSize, Paths),
    words_at_paths(Board, Paths, Words).

row_in_domain(Row) :- Row ins { 0'X, 0'M, 0'A, 0'S }.

board_of_size(Board, NumBoard-NumCols) :-
    length(Board, NumBoard),
    maplist({NumCols}/[Row]>>(length(Row, NumCols), row_in_domain(Row)), Board).

word_instantiated([A, B, C, D]) :- \+ var(A), \+ var(B), \+ var(C), \+ var(D).

definitely_a_solution(Word) :-
    word_instantiated(Word),
    [A, B, C, D] = Word,
    A #= 0'X,
    B #= 0'M,
    C #= 0'A,
    D #= 0'S.

definitely_not_a_solution(Word) :-
    word_instantiated(Word),
    [A, B, C, D] = Word,
    (
        A #\= 0'X
    ;   B #\= 0'M
    ;   C #\= 0'A
    ;   D #\= 0'S
    ).

%!  solved_word_combination(WordSolvedPairs, NumSolutions) is undefined.
%
%   This will generate lists of Word-Solved pairs (one for each word) with all different combinations of which ones are
%   solved so that they add up to the correct number of solutions. It also constrains them as much as possible.
%
%   FIXME -- this loops infinitely if you try to create a board with an impossible number of solutions, e.g. 4-4 with 7
%   solutions. Need to figure out how to fix this.
solved_word_combination([], 0) :- !.

solved_word_combination([], _NumSolutions) :- !, fail.

solved_word_combination([Word-0 | More], 0) :-
    !,
    \+ definitely_a_solution(Word),
    solved_word_combination(More, 0).

solved_word_combination([Word-0 | More], NumSolutions) :-
    % the current solution can be unsolved iff there are enough remaining solutions to fill in the rest of the
    % solutions.
    length(More, NumMoreWords),
    NumMoreWords #>= NumSolutions,
    \+ definitely_a_solution(Word),
    solved_word_combination(More, NumSolutions).

solved_word_combination([Word-1 | More], NumSolutions) :-
    % solution(Word),
    label(Word),
    definitely_a_solution(Word),
    NewNumSolutions #= NumSolutions - 1,
    length(More, NumMoreWords),
    NumMoreWords #>= NewNumSolutions,
    solved_word_combination(More, NewNumSolutions).

word_solved_status(Word, Solved) :-
    definitely_a_solution(Word)
->  Solved #= 1
;   (
        definitely_not_a_solution(Word)
    ->  Solved #= 0
    ;   true
    ).

word_solved_pairs(Words, WordSolvedPairs) :-
    maplist([Word, Word-Solved]>>word_solved_status(Word, Solved), Words, WordSolvedPairs).

board(Board, NumSolutions) :-
    board_of_size(Board, BoardSize),
    board(Board, BoardSize, NumSolutions).

board(Board, BoardSize, NumSolutions) :-
    board_of_size(Board, BoardSize),
    words(Board, BoardSize, Words),
    word_solved_pairs(Words, WordSolvedPairs),
    !,
    solved_word_combination(WordSolvedPairs, NumSolutions),
    % writeln(WordSolvedPairs),
    maplist(label, Board),
    % writeln(Board),
    % check that the correct number of things is solved one more time just to be sure we didn't accidentally introduce
    % any new solutions when filling in the board.
    solved_word_combination(WordSolvedPairs , NumSolutions).

write_board(Board) :-
    maplist([Row]>>(string_codes(S, Row), writeln(S)), Board).

parse_input([], []).

parse_input(Chars, [Row | MoreBoard]) :-
    append(Row, [0'\n | More], Chars)
    -> parse_input(More, MoreBoard)
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

solve_example(NumSolutions) :-
    example(Input),
    parse_input(Input, Board),
    board(Board, NumSolutions).

debug_solutions(Solution) :-
    example(Input),
    parse_input(Input, Board),
    board_of_size(Board, BoardSize),
    word_path(BoardSize, Path),
    word(Board, Path, Word),
    % Path = [ 6-9 | _ ],
    solution(Word),
    word_path_at_point_in_direction(BoardSize, Point, Direction, Path),
    string_codes(WordString, Word),
    Solution = [Point, Direction, Path, WordString].

profile_board(Board, BoardSize, NumSolutions, Time) :-
    statistics(runtime, [Start|_]),
    once(board(Board, BoardSize, NumSolutions)),
    statistics(runtime, [End|_]),
    write_board(Board),
    Time #= End - Start.

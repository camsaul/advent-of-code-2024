:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(clpfd)).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_strings/2]).

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(re_compile, true).

num_blinks(N, N) :- number(N).
num_blinks(part1, 25).
num_blinks(part2, 75).

path(example, 'day-11-example.txt').
path(actual, 'day-11.txt').

%! solve_stone(Stone, NumBlinks, Out) is det.

solve_stone(_Stone, 0, 1) :- !.

solve_stone("0", N, Out) :- !, NextN #= N - 1, solve_stone("1", NextN, Out).

% slight optimization: check if the first character is zero; we only need to roundtrip between number and string if it
% is.
trim_leading_zeroes(String0, String) :-
    string_code(1, String0, 0'0)
->  number_string(N, String0),
    number_string(N, String)
;   String = String0.

:- table solve_stone/3.

split_stone(Stone, Length, Left, Right) :-
    HalfLength #= Length // 2,
    sub_string(Stone, 0, HalfLength, HalfLength, Left),
    sub_string(Stone, HalfLength, HalfLength, 0, Right0),
    trim_leading_zeroes(Right0, Right).

solve_stone(Stone, N, Out) :-
    string_length(Stone, Length),
    Length mod 2 #= 0,
    !,
    split_stone(Stone, Length, Left, Right),
    NextN #= N - 1,
    solve_stone(Left, NextN, LeftOut),
    solve_stone(Right, NextN, RightOut),
    Out #= LeftOut + RightOut.

solve_stone(Stone, N, Out) :-
    number_string(StoneNum, Stone),
    NextStoneNum #= StoneNum * 2024,
    number_string(NextStoneNum, NextStone),
    NextN #= N - 1,
    solve_stone(NextStone, NextN, Out).

solve_stones(NumBlinks, Stones, Result) :-
    maplist({NumBlinks}/[Stone, Out]>>solve_stone(Stone, NumBlinks, Out), Stones, Results),
    foldl([N, Acc0, Acc1]>>(Acc1 #= Acc0 + N), Results, 0, Result),
    !.

stones(Source, Stones) :-
    path(Source, Path),
    read_file_lines_to_strings(Path, [Line]),
    split_string(Line, ' ', '', Stones).

% solve(example, part1, N).
% solve(actual, part2, N).
solve(Source, Part, N) :- stones(Source, Stones), num_blinks(Part, NumBlinks), solve_stones(NumBlinks, Stones, N), !.

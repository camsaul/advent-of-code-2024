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

:- table solve_stone/3.

solve_stone(_Stone, 0, 1) :- !.

solve_stone(0, N, Out) :- !, NextN #= N - 1, solve_stone(1, NextN, Out).

string_halves(Str, Length, Left, Right) :-
    HalfLength #= Length // 2,
    sub_string(Str, 0, HalfLength, HalfLength, Left),
    sub_string(Str, HalfLength, HalfLength, 0, Right).

solve_stone(Stone, N, Out) :-
    number_string(Stone, Str),
    string_length(Str, Length),
    Length mod 2 #= 0,
    !,
    string_halves(Str, Length, LeftStr, RightStr),
    number_string(Left, LeftStr),
    number_string(Right, RightStr),
    NextN #= N - 1,
    solve_stone(Left, NextN, LeftOut),
    solve_stone(Right, NextN, RightOut),
    Out #= LeftOut + RightOut.

solve_stone(Stone, N, Out) :-
    NextStone #= Stone * 2024,
    NextN #= N - 1,
    solve_stone(NextStone, NextN, Out).

solve_stones(NumBlinks, Stones, Result) :-
    maplist({NumBlinks}/[Stone, Out]>>solve_stone(Stone, NumBlinks, Out), Stones, Results),
    foldl([N, Acc0, Acc1]>>(Acc1 #= Acc0 + N), Results, 0, Result),
    !.

stones(Source, Stones) :-
    path(Source, Path),
    read_file_lines_to_strings(Path, [Line]),
    split_string(Line, ' ', '', StoneStrings),
    maplist([S, N]>>number_string(N, S), StoneStrings, Stones).

% solve(example, part1, N).
% solve(actual, part2, N).
solve(Source, Part, N) :- stones(Source, Stones), num_blinks(Part, NumBlinks), solve_stones(NumBlinks, Stones, N), !.

:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3]).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_strings/2]).

% num_stones_after_blinks(Stone, NumBlinks, NumStones).

num_stones_after_blinks(_Stone, 0, 1) :- !.

num_stones_after_blinks(0, NumBlinks, NumStones) :-
    !,
    NextStone #= 1,
    NextNumBlinks #= NumBlinks - 1,
    num_stones_after_blinks(NextStone, NextNumBlinks, NumStones).

num_stones_after_blinks(N, NumBlinks, NumStones) :-
    number_chars(N, Chars),
    length(Chars, Length),
    Length mod 2 #= 0,
    !,
    HalfLength #= Length // 2,
    length(LeftChars, HalfLength),
    length(RightChars, HalfLength),
    append(LeftChars, RightChars, Chars),
    number_chars(Left, LeftChars),
    number_chars(Right, RightChars),
    NextNumBlinks #= NumBlinks - 1,
    num_stones_after_blinks(Left, NextNumBlinks, LeftNumStones),
    num_stones_after_blinks(Right, NextNumBlinks, RightNumStones),
    NumStones #= LeftNumStones + RightNumStones.

num_stones_after_blinks(N, NumBlinks, NumStones) :-
    NextNumBlinks #= NumBlinks - 1,
    NextStone #= N * 2024,
    num_stones_after_blinks(NextStone, NextNumBlinks, NumStones).

reduce_num_stones_after_blinks(NumBlinks, Stone, Acc0, Acc1) :-
    num_stones_after_blinks(Stone, NumBlinks, NumStones),
    Acc1 #= Acc0 + NumStones.

path(example, 'day-11-example.txt').
path(actual, 'day-11.txt').

stones(Source, Stones) :-
    path(Source, Path),
    read_file_lines_to_strings(Path, [Line]),
    split_string(Line, " ", "", NumStrs),
    maplist([S, N]>>number_string(N, S), NumStrs, Stones).

solve(Stones, NumBlinks, NumStones) :- foldl(call(reduce_num_stones_after_blinks(NumBlinks)), Stones, 0, NumStones).

% solve(example, N).
% solve(actual, N).
solve(Source, N) :- stones(Source, Stones), solve(Stones, 25, N).

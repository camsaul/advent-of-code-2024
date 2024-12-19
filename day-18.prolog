:- module('day-18', [solve_part_1/2, solve_part_2/2]).

:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, nth0/3]).
:- use_module(library(yall)).

:- use_module(a_star, [a_star/5]).
:- use_module(bitset_grid_util, [xy_absolute_position/3, next_abs_position/4]).
:- use_module(util, [read_file_to_chars/2, bitset_is_set/2, bitset_set/4]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).

%
% File parsing code
%

path(example, `day-18-example.txt`).
path(actual, `day-18.txt`).

size(example, 7-7).
size(actual, 71-71).

part_1_num_bytes(example, 12).
part_1_num_bytes(actual, 1024).

start_position(_, 0).
end_position(Input, EndPosition) :- size(Input, Width-Height), EndPosition #= (Width * Height) - 1.

digit(Char) --> [Char], { char_type(Char, digit) }.

digits([]) --> [].
digits([Char|More]) --> digit(Char), digits(More).

number(N) --> digits([X|More]), { number_chars(N, [X|More]) }.

coordinate(X-Y) --> number(X), ",", number(Y).

coordinates([]) --> [].
coordinates([XY]) --> coordinate(XY).
coordinates([XY|More]) --> coordinate(XY), "\n", coordinates(More).

file(Coordinates) --> coordinates(Coordinates), "\n".

walls_bitset(Input, AllCoordinates, NumBytes, Walls) :-
    size(Input, Width-_),
    length(Coordinates, NumBytes),
    append(Coordinates, _, AllCoordinates),
    foldl({Width}/[XY, BitSet0, BitSet1]>>(
                                              xy_absolute_position(Width, XY, Pos),
                                              bitset_set(BitSet0, Pos, 1, BitSet1)
                                          ),
          Coordinates,
          0,
          Walls).

input_coordinates(Input, Coordinates) :- path(Input, Path), read_file_to_chars(Path, Chars), phrase(file(Coordinates), Chars).

init_walls_part_1(Input, Walls) :-
    input_coordinates(Input, Coordinates),
    part_1_num_bytes(Input, NumBytes),
    walls_bitset(Input, Coordinates, NumBytes, Walls).

init_part_1(Input, Walls, Size, StartPosition, EndPosition) :-
    init_walls_part_1(Input, Walls),
    size(Input, Size),
    start_position(Input, StartPosition),
    end_position(Input, EndPosition).

%
% solution
%

next_valid_abs_position(Size, Walls, P, NextP) :- next_abs_position(Size, _Direction, P, NextP), \+ bitset_is_set(Walls, NextP).

solve_part_1(Input, N) :-
    init_part_1(Input, Walls, Size, StartPosition, EndPosition),
    a_star(Size, call(next_valid_abs_position(Size, Walls)), StartPosition, EndPosition, Path),
    length(Path, Length),
    % don't count the first node in the path.
    N #= Length - 1.

%
% part2
%

binary_search_num_bytes(Input, Coordinates, MinNumBytes, CurrentNumBytes, MaxNumBytes, SolutionNumBytes) :-
    format(`~w...~w...~w~n`, [MinNumBytes, CurrentNumBytes, MaxNumBytes]),
    MinNumBytes #< CurrentNumBytes,
    CurrentNumBytes #< MaxNumBytes,
    size(Input, Size),
    walls_bitset(Input, Coordinates, CurrentNumBytes, Walls),
    start_position(Input, StartPosition),
    end_position(Input, EndPosition),
    (
        a_star(Size, call(next_valid_abs_position(Size, Walls)), StartPosition, EndPosition, _Path)
    ->  NextMin = CurrentNumBytes,
        NextCurrent #= ((MaxNumBytes - CurrentNumBytes) // 2) + CurrentNumBytes,
        NextMax = MaxNumBytes
    ;   NextMin = MinNumBytes,
        NextCurrent #= ((CurrentNumBytes - MinNumBytes) // 2) + MinNumBytes,
        NextMax = CurrentNumBytes
    ),
    (
        NextCurrent #= CurrentNumBytes
    ->  SolutionNumBytes #= CurrentNumBytes
    ;   binary_search_num_bytes(Input, Coordinates, NextMin, NextCurrent, NextMax, SolutionNumBytes)
    ).

solve_part_2(Input, Solution) :-
    input_coordinates(Input, Coordinates),
    !,
    part_1_num_bytes(Input, MinNumBytes),
    length(Coordinates, MaxNumBytes),
    CurrentNumBytes #= (MaxNumBytes // 2) + MinNumBytes,
    binary_search_num_bytes(Input, Coordinates, MinNumBytes, CurrentNumBytes, MaxNumBytes, SolutionNumBytes),
    nth0(SolutionNumBytes, Coordinates, Solution).

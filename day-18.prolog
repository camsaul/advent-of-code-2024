:- use_module(library(apply), [foldl/4, maplist/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3]).
:- use_module(library(yall)).

:- use_module(bitset_grid_util, [xy_absolute_position/3, next_abs_position/4]).
:- use_module(util, [read_file_to_chars/2, bitset_is_set/2, bitset_set/4]).
:- use_module(a_star, [a_star/5]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).

% move_forward_cost(1).
% turn_cost(0).

%
% File parsing code
%

path(example, `day-18-example.txt`).
path(actual, `day-18.txt`).

size(example, 7-7).
size(actual, 71-71).

num_bytes(example, 12).
num_bytes(actual, 1024).

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

walls_bitset(Input, AllCoordinates, Walls) :-
    size(Input, Width-_),
    num_bytes(Input, NumBytes),
    length(Coordinates, NumBytes),
    append(Coordinates, _, AllCoordinates),
    foldl({Width}/[XY, BitSet0, BitSet1]>>(
                                              xy_absolute_position(Width, XY, Pos),
                                              bitset_set(BitSet0, Pos, 1, BitSet1)
                                          ),
          Coordinates,
          0,
          Walls).

init_walls(Input, Walls) :-
    path(Input, Path),
    read_file_to_chars(Path, Chars),
    phrase(file(Coordinates), Chars),
    walls_bitset(Input, Coordinates, Walls).

:- table init/5.

init(Input, Walls, Size, StartPosition, EndPosition) :-
    init_walls(Input, Walls),
    size(Input, Size),
    start_position(Input, StartPosition),
    end_position(Input, EndPosition).

%
% solution
%

next_valid_abs_position(Size, Walls, P, NextP) :- next_abs_position(Size, _Direction, P, NextP), \+ bitset_is_set(Walls, NextP).

solve(Input, N) :-
    init(Input, Walls, Size, StartPosition, EndPosition),
    % format('Walls = 0b~2r~n', [Walls]),
    a_star(Size, call(next_valid_abs_position(Size, Walls)), StartPosition, EndPosition, Path),
    length(Path, Length),
    % format('Path = ~w~n', [Path]),
    % Size = Width-_,
    % maplist({Width}/[P, XY]>>xy_absolute_position(Width, XY, P), Path, Path1),
    % format('Path1 = ~w~n', [Path1]),
    % don't count the first node in the path.
    N #= Length - 1.

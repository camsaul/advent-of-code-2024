:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [maplist/2, foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2]).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_chars/2]).

:- set_prolog_flag(double_quotes, chars).

absolute_position(Width, X-Y, Pos) :-
    MaxX #= Width - 1,
    X in 0..MaxX,
    Y #>= 0,
    Pos #= X + (Y * Width).

set_bit(N0, Index, V, N1) :-
    V in { 0, 1 },
    Existing is getbit(N0, Index),
    (
        Existing #= V
    ->  N1 #= N0
    ;   BitMask #= 1 << Index,
        N1 #= N0 xor BitMask
    ).

next_position(Width,  '^', Position0, Position1) :- Position1 #= Position0 - Width.
next_position(Width,  'v', Position0, Position1) :- Position1 #= Position0 + Width.
next_position(_Width, '<', Position0, Position1) :- Position1 #= Position0 - 1.
next_position(_Width, '>', Position0, Position1) :- Position1 #= Position0 + 1.

move_package(Width, Direction, Walls, PackagePosition, Packages0, Packages) :-
    \+ object_at_position(PackagePosition, Packages0)
->  Packages #= Packages0
;   next_position(Width, Direction, PackagePosition, NextPosition),
    \+ object_at_position(NextPosition, Walls),
    set_bit(Packages0, PackagePosition, 0, Packages1),
    move_package(Width, Direction, Walls, NextPosition, Packages1, Packages2),
    set_bit(Packages2, NextPosition, 1, Packages).

move_robot(Width, Direction, Walls, Packages0, RobotPosition0, Packages, RobotPosition) :-
    next_position(Width, Direction, RobotPosition0, RobotPosition1),
    (
        (
            \+ object_at_position(RobotPosition1, Walls),
            RobotPosition #= RobotPosition1,
            move_package(Width, Direction, Walls, RobotPosition1, Packages0, Packages)
        )
    ->  true
    ;   Packages #= Packages0,
        RobotPosition #= RobotPosition0
    ).

move(Width, Directions, Walls, Packages0, RobotPosition0, Packages, RobotPosition) :-
    foldl({Width, Walls}/[Dir, P0-R0, P1-R1]>>move_robot(Width, Dir, Walls, P0, R0, P1, R1),
          Directions,
          Packages0-RobotPosition0,
          Packages-RobotPosition).

object_at_position(AbsolutePosition, Objects) :- 1 is getbit(Objects, AbsolutePosition).

parse_walls([], _AbsolutePosition, Walls, Walls).

parse_walls([Char|More], AbsolutePosition, Walls0, Walls) :-
    (
        Char = '#'
    ->  set_bit(Walls0, AbsolutePosition, 1, Walls1)
    ;   Walls1 #= Walls0
    ),
    NextPosition #= AbsolutePosition + 1,
    parse_walls(More, NextPosition, Walls1, Walls).

parse_packages([], _AbsolutePosition, Packages, Packages).

parse_packages([Char|More], AbsolutePosition, Packages0, Packages) :-
    (
        Char = 'O'
    ->  set_bit(Packages0, AbsolutePosition, 1, Packages1)
    ;   Packages1 #= Packages0
    ),
    NextPosition #= AbsolutePosition + 1,
    parse_packages(More, NextPosition, Packages1, Packages).

robot_position([], Position, Position) :- fail.

robot_position([Char|More], Position0, Position) :-
    Char = '@'
->  Position = Position0
;   NextPosition #= Position0 + 1,
    robot_position(More, NextPosition, Position).

parse_input(Input, Walls, Packages, RobotPosition) :-
    parse_walls(Input, 0, 0, Walls),
    parse_packages(Input, 0, 0, Packages),
    robot_position(Input, 0, RobotPosition).

split_file_lines(Lines, RoomInputLines, DirectionsLines) :- append([RoomInputLines, [[]], DirectionsLines], Lines).

read_file(Path, Width, RoomInput, Directions) :-
    read_file_lines_to_chars(Path, Lines),
    split_file_lines(Lines, RoomLines, DirectionsLines),
    [FirstRoomLine|_] = RoomLines,
    length(FirstRoomLine, Width),
    append(RoomLines, RoomInput),
    append(DirectionsLines, Directions).

set_bit_index(N, Offset, Index) :-
    N #> 0,
    LSBIndex is lsb(N),
    Index0 #= Offset + LSBIndex,
    (
        Index #= Index0
    ;   NextN is N >> ( LSBIndex + 1),
        NextIndex #= Index0 + 1,
        set_bit_index(NextN, NextIndex, Index)
    ).

set_bit_index(N, Index) :- set_bit_index(N, 0, Index).

package_gps_coodinate(Width, AbsolutePosition, Coordinate) :-
    absolute_position(Width, X-Y, AbsolutePosition),
    Coordinate #= (Y * 100) + X.

path(example, 'day-15-example.txt').
path(actual, 'day-15.txt').

input(Input, Width, Walls, Packages, RobotPosition, Directions) :-
    path(Input, Path),
    read_file(Path, Width, RoomInput, Directions),
    parse_input(RoomInput, Walls, Packages, RobotPosition).

solve(Input, Out) :-
    input(Input, Width, Walls, Packages, RobotPosition, Directions),
    move(Width, Directions, Walls, Packages, RobotPosition, Packages1, _RobotPosition1),
    !,
    aggregate_all(sum(Coordinate),
                  (
                      set_bit_index(Packages1, Position),
                      package_gps_coodinate(Width, Position, Coordinate)
                  ),
                  Out).

%
% Printing code for debugging
%

position(Width-Height, X-Y) :-
    MaxX #= Width - 1,
    MaxY #= Height - 1,
    X in 0..MaxX,
    Y in 0..MaxY.

print_position(Width, Walls, Packages, RobotPosition, AbsolutePosition) :-
    (
        AbsolutePosition mod Width #= 0
    ->  nl
    ;   true
    ),
    print_position(Walls, Packages, RobotPosition, AbsolutePosition).

print_position(Walls, Packages, RobotPosition, AbsolutePosition) :-
    object_at_position(AbsolutePosition, Walls)
->  write('#')
;   object_at_position(AbsolutePosition, Packages)
->  write('0')
;   RobotPosition #= AbsolutePosition
->  write('@')
;   write('.').

print_room(Width-Height, Walls, Packages, RobotPosition) :-
    findall(AbsolutePosition,
            (
                position(Width-Height, X-Y),
                label([Y, X]),
                absolute_position(Width, X-Y, AbsolutePosition)
            ),
            AbsolutePositions),
    maplist(call(print_position(Width, Walls, Packages, RobotPosition)),
            AbsolutePositions),
    nl.

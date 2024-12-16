:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2]).

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

package_at_position(PackagePosition, Packages, left_half) :- object_at_position(PackagePosition, Packages).

package_at_position(PackagePosition, Packages, right_half) :-
    LeftPosition = PackagePosition - 1,
    object_at_position(LeftPosition, Packages).

move_package(Width, Direction, Walls, PackagePosition, Packages0, Packages) :-
    \+ package_at_position(PackagePosition, Packages0, PackageHalf)
->  % no package at position
    Packages #= Packages0
;   package_at_position(PackagePosition, Packages0, PackageHalf),
    (
        PackageHalf = left_half
    ->  LeftPosition #= PackagePosition,
        RightPosition #= PackagePosition + 1
    ;   LeftPosition #= PackagePosition - 1,
        RightPosition #= PackagePosition
    ),
    next_position(Width, Direction, LeftPosition, NextLeftPosition),
    next_position(Width, Direction, RightPosition, NextRightPosition),
    % check for walls
    \+ object_at_position(NextLeftPosition, Walls),
    \+ object_at_position(NextRightPosition, Walls),
    % move left half into the ether
    set_bit(Packages0, LeftPosition, 0, Packages1),
    % (recursively) move both sides
    move_package(Width, Direction, Walls, NextLeftPosition, Packages1, Packages2),
    move_package(Width, Direction, Walls, NextRightPosition, Packages2, Packages3),
    % put left half into its new position
    set_bit(Packages3, NextLeftPosition, 1, Packages).

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

move(_Size, [], _Walls, Packages, RobotPosition, Packages, RobotPosition).

move(Width-Height, [Direction|MoreDirections], Walls, Packages0, RobotPosition0, Packages, RobotPosition) :-
    % print_room(Width-Height, Walls, Packages0, RobotPosition0),
    move_robot(Width, Direction, Walls, Packages0, RobotPosition0, Packages1, RobotPosition1),
    move(Width-Height, MoreDirections, Walls, Packages1, RobotPosition1, Packages, RobotPosition).

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
        Char = '['
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

read_file(Path, Width-Height, RoomInput, Directions) :-
    read_file_lines_to_chars(Path, Lines),
    split_file_lines(Lines, RoomLines, DirectionsLines),
    length(RoomLines, Height),
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

transform_input('#', "##").
transform_input('O', "[]").
transform_input('.', "..").
transform_input('@', "@.").

expand_room([], []).

expand_room([Char|MoreIn], [X, Y | MoreOut]) :- transform_input(Char, [X, Y]), expand_room(MoreIn, MoreOut).

input(Input, Size, Walls, Packages, RobotPosition, Directions) :-
    path(Input, Path),
    read_file(Path, Width0-Height, RoomInput0, Directions),
    expand_room(RoomInput0, RoomInput),
    Width #= Width0 * 2,
    Size = Width-Height,
    parse_input(RoomInput, Walls, Packages, RobotPosition).

solve(Input, Out) :-
    input(Input, Size, Walls, Packages, RobotPosition, Directions),
    move(Size, Directions, Walls, Packages, RobotPosition, Packages1, _RobotPosition1),
    Width-_Height = Size,
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
->  write('[')
;   (
        LeftPosition #= AbsolutePosition - 1,
        object_at_position(LeftPosition, Packages)
    )
->  write(']')
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

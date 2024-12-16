:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2]).
:- use_module(library(yall)).

:- use_module(bitset_grid_util, [absolute_position/2,
                                 next_absolute_position/4,
                                 xy_absolute_position/3]).
:- use_module(util, [read_file_lines_to_chars/2,
                     bitset_set/4,
                     bitset_is_set/2]).

:- set_prolog_flag(double_quotes, chars).

char_direction(^, up).
char_direction(v, down).
char_direction(<, left).
char_direction(>, right).

next_position(Width, CharDirection, AbsolutePosition, NextAbsolutePosition) :-
    char_direction(CharDirection, Direction),
    next_absolute_position(Width, Direction, AbsolutePosition, NextAbsolutePosition).

package_at_position(PackagePosition, Packages, left_half) :- bitset_is_set(Packages, PackagePosition).

package_at_position(PackagePosition, Packages, right_half) :-
    LeftPosition = PackagePosition - 1,
    bitset_is_set(Packages, LeftPosition).

move_package(Width, Direction, Walls, PackagePosition, Packages0, Packages) :-
    \+ package_at_position(PackagePosition, Packages0, PackageHalf)
->  Packages #= Packages0 % no package at position
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
    \+ bitset_is_set(Walls, NextLeftPosition),
    \+ bitset_is_set(Walls, NextRightPosition),
    % move left half into the ether
    bitset_set(Packages0, LeftPosition, 0, Packages1),
    % (recursively) move both sides
    move_package(Width, Direction, Walls, NextLeftPosition, Packages1, Packages2),
    move_package(Width, Direction, Walls, NextRightPosition, Packages2, Packages3),
    % put left half into its new position
    bitset_set(Packages3, NextLeftPosition, 1, Packages).

move_robot(Width, Direction, Walls, Packages0, RobotPosition0, Packages, RobotPosition) :-
    next_position(Width, Direction, RobotPosition0, RobotPosition1),
    (
        (
            \+ bitset_is_set(Walls, RobotPosition1),
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

parse_walls([], _AbsolutePosition, Walls, Walls).

parse_walls(['#'|More], AbsolutePosition, Walls0, Walls) :-
    bitset_set(Walls0, AbsolutePosition, 1, Walls1),
    NextPosition #= AbsolutePosition + 1,
    parse_walls(More, NextPosition, Walls1, Walls).

parse_walls([Char|More], AbsolutePosition, Walls0, Walls) :-
    Char \= '#',
    NextPosition #= AbsolutePosition + 1,
    parse_walls(More, NextPosition, Walls0, Walls).

parse_packages([], _AbsolutePosition, Packages, Packages).

parse_packages(['['|More], AbsolutePosition, Packages0, Packages) :-
    bitset_set(Packages0, AbsolutePosition, 1, Packages1),
    NextPosition #= AbsolutePosition + 1,
    parse_packages(More, NextPosition, Packages1, Packages).

parse_packages([Char|More], AbsolutePosition, Packages0, Packages) :-
    Char \= '[',
    NextPosition #= AbsolutePosition + 1,
    parse_packages(More, NextPosition, Packages0, Packages).

robot_position([], Position, Position) :- fail.

robot_position(['@'|_], Position, Position).

robot_position([Char|More], Position0, Position) :-
    Char \= '@',
    NextPosition #= Position0 + 1,
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

package_gps_coodinate(Width, AbsolutePosition, Coordinate) :-
    xy_absolute_position(Width, X-Y, AbsolutePosition),
    Coordinate #= (Y * 100) + X.

path(example, 'day-15-example.txt').
path(actual, 'day-15.txt').

transform_input('#', "##").
transform_input('O', "[]").
transform_input('.', "..").
transform_input('@', "@.").

expand_room([], []).

expand_room([Char|MoreIn], [X, Y | MoreOut]) :- transform_input(Char, [X, Y]), expand_room(MoreIn, MoreOut).

input(Input, Width, Walls, Packages, RobotPosition, Directions) :-
    path(Input, Path),
    read_file(Path, Width0, RoomInput0, Directions),
    expand_room(RoomInput0, RoomInput),
    Width #= Width0 * 2,
    parse_input(RoomInput, Walls, Packages, RobotPosition).

solve(Input, Out) :-
    input(Input, Width, Walls, Packages, RobotPosition, Directions),
    move(Width, Directions, Walls, Packages, RobotPosition, Packages1, _RobotPosition1),
    !,
    % Height #= Width // 2,
    % print_room(Width-Height, Walls, Packages1, RobotPosition1),
    aggregate_all(sum(Coordinate),
                  (
                      bitset_is_set(Packages1, Position),
                      package_gps_coodinate(Width, Position, Coordinate)
                  ),
                  Out).

%
% Printing code for debugging
%

print_position(Walls, Packages, RobotPosition, AbsolutePosition) :-
    bitset_is_set(Walls, AbsolutePosition)
->  write('#')
;   bitset_is_set(Packages, AbsolutePosition)
->  write('[')
;   (
        LeftPosition #= AbsolutePosition - 1,
        bitset_is_set(Packages, LeftPosition)
    )
->  write(']')
;   RobotPosition #= AbsolutePosition
->  write('@')
;   write('.').

print_position(Width, Walls, Packages, RobotPosition, AbsolutePosition) :-
    (
        AbsolutePosition mod Width #= 0
    ->  nl
    ;   true
    ),
    print_position(Walls, Packages, RobotPosition, AbsolutePosition).

print_room(Width-Height, Walls, Packages, RobotPosition) :-
    forall((absolute_position(Width-Height, AbsolutePosition), label([AbsolutePosition])),
           print_position(Width, Walls, Packages, RobotPosition, AbsolutePosition)),
    nl.

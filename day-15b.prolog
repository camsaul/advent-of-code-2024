:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3]).
:- use_module(library(yall)).

:- use_module(bitset_grid_util, [absolute_position/2,
                                 next_absolute_position/4,
                                 xy_absolute_position/3]).
:- use_module(util, [read_file_to_chars/2,
                     bitset_set/4,
                     bitset_is_set/2,
                     goal_bitset/3,
                     first_index/3]).

:- set_prolog_flag(double_quotes, chars).

char_direction(^, up).
char_direction(v, down).
char_direction(<, left).
char_direction(>, right).

next_position(Width, Direction, AbsolutePosition, NextAbsolutePosition) :-
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

parse_input(Input, Walls, Packages, RobotPosition) :-
    goal_bitset(call(=(wall)), Input, Walls),
    goal_bitset(call(=(package_left)), Input, Packages),
    first_index(Input, call(=(robot)), RobotPosition).

char_entity(#, wall).
char_entity(., empty).
char_entity('O', package).
char_entity(@, robot).

transform_entity(wall,    [wall, wall]).
transform_entity(package, [package_left, package_right]).
transform_entity(empty,   [empty, empty]).
transform_entity(robot,   [robot, empty]).

grid_character(Out) --> [X], { char_entity(X, Entity), transform_entity(Entity, Out) }.

grid_characters([]) --> [].
grid_characters([X,Y|More]) --> grid_character([X,Y]), grid_characters(More).

grid([], 0-0) --> [].
grid(Grid, Width-Height) -->
    grid_characters(X),
    "\n",
    grid(More, _Width-Height0),
    {
        append(X, More, Grid),
        length(X, Width),
        Height is Height0 + 1
    }.

direction(Direction) --> [X], { char_direction(X, Direction) }.

directions([]) --> [].
directions([X|More]) --> direction(X), directions(More).
directions(Directions) --> "\n", directions(Directions).

file(Grid, Size, Directions) --> grid(Grid, Size), "\n", directions(Directions).

read_file(Path, Width, RoomInput, Directions) :-
    read_file_to_chars(Path, Chars),
    phrase(file(RoomInput, Width-_Height, Directions), Chars).

path(example, 'day-15-example.txt').
path(actual, 'day-15.txt').

input(Input, Width, Walls, Packages, RobotPosition, Directions) :-
    path(Input, Path),
    read_file(Path, Width, RoomInput, Directions),
    parse_input(RoomInput, Walls, Packages, RobotPosition).

package_gps_coodinate(Width, AbsolutePosition, Coordinate) :-
    xy_absolute_position(Width, X-Y, AbsolutePosition),
    Coordinate #= (Y * 100) + X.

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

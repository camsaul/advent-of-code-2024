:- use_module(library(clpfd)).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(lists), [member/2]).

:- use_module(util, [read_file_lines_to_strings/2]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(pcre), [re_matchsub/3]).
:- use_module(library(yall)).

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(re_compile, true).

size(actual, [101, 103]).
size(example, [11, 7]).

num_moves(100).

quadrant([top, left]).
quadrant([top, right]).
quadrant([bottom, left]).
quadrant([bottom, right]).

move_robot([Width, Height], N, robot{p:[PX, PY], v: [VX, VY]}, robot{p:[NewX, NewY], v: [VX, VY]}) :-
    MaxX #= Width - 1,
    PX in 0..MaxX,
    MaxY #= Height - 1,
    PY in 0..MaxY,
    NewX #= (PX + (VX * N)) mod Width,
    NewY #= (PY + (VY * N)) mod Height.

move_robots(Size, NMoves, Robots, MovedRobots) :- maplist(call(move_robot(Size, NMoves)), Robots, MovedRobots).

quadrant_x_position([Width, _Height], left, X) :-
    MaxX #= (Width // 2) - 1,
    X in 0..MaxX.

quadrant_x_position([Width, _Height], right, X) :-
    Start #= (Width // 2) + 1,
    MaxX #= Width - 1,
    X in Start..MaxX.

quadrant_y_position([_Width, Height], top, Y) :-
    MaxY #= (Height // 2) - 1,
    Y in 0..MaxY.

quadrant_y_position([_Width, Height], bottom, Y) :-
    Start #= (Height // 2) + 1,
    MaxY #= Height - 1,
    Y in Start..MaxY.

quadrant_position(Size, [VerticalHalf, HorizontalHalf], [X, Y]) :-
    quadrant_x_position(Size, HorizontalHalf, X),
    quadrant_y_position(Size, VerticalHalf, Y).

quadrant_robot(Size, Quadrant, Robot) :- quadrant_position(Size, Quadrant, Robot.p).

num_quadrant_robots(Size, Quadrant, AllRobots, NumRobots) :-
    aggregate_all(sum(N),
                  (member(Robot, AllRobots), quadrant_robot(Size, Quadrant, Robot), N #= 1),
                  NumRobots).

bathroom_safety_factor(Size, AllRobots, TotalSafetyFactor) :-
    findall(SafetyFactor,
          (
              quadrant(Quadrant),
              num_quadrant_robots(Size, Quadrant, AllRobots, SafetyFactor),
              format('Quadrant = ~w~n', [Quadrant]),
              format('SafetyFactor = ~w~n', [SafetyFactor])
          ),
          SafetyFactors),
    format('SafetyFactors = ~w~n', [SafetyFactors]),
    foldl([X, Acc0, Acc1]>>(Acc1 #= Acc0 * X), SafetyFactors, 1, TotalSafetyFactor).

path(example, 'day-14-example.txt').
path(actual, 'day-14.txt').

robot_string(String, robot{p:[PX, PY], v:[VX, VY]}) :-
    re_matchsub("p=([-\\d]+),([-\\d]+) v=([-\\d]+)\\,([-\\d]+)", String, Match),
    number_string(PX, Match.1),
    number_string(PY, Match.2),
    number_string(VX, Match.3),
    number_string(VY, Match.4).

robots(Input, Robots) :-
    path(Input, Path),
    read_file_lines_to_strings(Path, Lines),
    maplist(robot_string, Lines, Robots).

solve(Input, SafetyFactor) :-
    size(Input, Size),
    robots(Input, Robots),
    num_moves(NMoves),
    move_robots(Size, NMoves, Robots, MovedRobots),
    bathroom_safety_factor(Size, MovedRobots, SafetyFactor).

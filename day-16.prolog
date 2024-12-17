:- use_module(library(clpfd)).

:- use_module(util, [read_file_to_chars/2,
                     goal_bitset/3,
                     first_index/3,
                     bitset_is_set/2,
                     bitset_set/4]).
:- use_module(bitset_grid_util, [absolute_position/2,
                                 next_absolute_position/4,
                                 xy_absolute_position/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(yall), [(>>)/5]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).
:- set_prolog_flag(occurs_check, error).

path(example, `day-16-example.txt`).
path(actual, `day-16.txt`).

%
% File parsing code
%

read_file(Input, Chars) :-
    path(Input, Path),
    read_file_to_chars(Path, Chars).

char_entity(#, wall).
char_entity(., empty).
char_entity('S', start).
char_entity('E', end).

grid_character(Out) --> [X], { char_entity(X, Out) }.

grid_characters([]) --> [].
grid_characters([X|More]) --> grid_character(X), grid_characters(More).

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

file(Grid, Size) --> grid(Grid, Size).

parse_file(Input, Walls, Size, StartPosition, EndPosition) :-
    read_file(Input, Chars),
    phrase(file(Grid, Size), Chars),
    goal_bitset(call(=(wall)), Grid, Walls),
    first_index(Grid, call(=(start)), StartPosition),
    first_index(Grid, call(=(end)), EndPosition).

%
% solution
%

config_walls(config(Walls, _, _, _), Walls).
config_size(config(_, Size, _, _), Size).
config_start_position(config(_, _, StartPosition, _), StartPosition).
config_end_position(config(_, _, _, EndPosition), EndPosition).

state_visited_nodes(state(VisitedNodes, _, _, _), VisitedNodes).
state_position(state(_, Position, _, _), Position).
state_direction(state(_, _, Direction, _), Direction).
state_cost(state(_, _, _, Cost), Cost).

dead_end(Width-Height, Walls, Position) :-
    absolute_position(Width-Height, Position),
    label([Position]),
    \+ bitset_is_set(Walls, Position),
    next_absolute_position(Width, left, Position, LeftPosition),
    next_absolute_position(Width, right, Position, RightPosition),
    next_absolute_position(Width, up, Position, UpPosition),
    next_absolute_position(Width, down, Position, DownPosition),
    Mask #= (1 << LeftPosition) \/ (1 << RightPosition) \/ (1 << UpPosition) \/ (1 << DownPosition),
    NumWalls #= popcount(Walls /\ Mask),
    NumWalls #= 3.

:- table(eliminate_dead_ends/5).

% This is a little optimization -- fill in all dead ends with walls, and then recursively fill them in until there are
% no more to fill.
eliminate_dead_ends(Size, StartPosition, EndPosition, Walls0, Walls) :-
    writeln(`Eliminating dead ends...`),
    findall(Position,
            (dead_end(Size, Walls0, Position), Position #\= StartPosition, Position #\= EndPosition),
            DeadEnds),
    (
        DeadEnds = []
    ->  % no dead ends, we're done
        Walls = Walls0
    ;   % otherwise fill in the dead ends with walls and recurse.
        foldl([Position, WallsIn, WallsOut]>>bitset_set(WallsIn, Position, 1, WallsOut),
              DeadEnds,
              Walls0,
              Walls1),
        eliminate_dead_ends(Size, StartPosition, EndPosition, Walls1, Walls)
    ).

%!   best_cost(Position, Direction, Cost)
:- dynamic(best_cost/3).

init(Input, config(Walls, Size, StartPosition, EndPosition), state(VisitedNodes, Position, Direction, Cost)) :-
    parse_file(Input, Walls0, Size, StartPosition, EndPosition),
    eliminate_dead_ends(Size, StartPosition, EndPosition, Walls0, Walls),
    bitset_set(0, StartPosition, 1, VisitedNodes),
    Position = StartPosition,
    Direction = right,
    Cost = 0,
    retractall(best_cost(_, _, _)).

at_end(Config, State) :- config_end_position(Config, P), state_position(State, P).

move_forward_or_turn(Config, StartState, EndState) :-
    move_forward(Config, StartState, EndState)
    ;   turn(left, Config, StartState, EndState)
    ;   turn(right, Config, StartState, EndState).

cannot_turn(config(Walls, Width-_, _, _), state(VisitedNodes, Position, Direction, _Cost)) :-
    turn(left, Direction, LHSDirection),
    next_absolute_position(Width, LHSDirection, Position, LHSPosition),
    (
        bitset_is_set(Walls, LHSPosition), !
    ;   bitset_is_set(VisitedNodes, LHSPosition)
    ),
    turn(right, Direction, RHSDirection),
    next_absolute_position(Width, RHSDirection, Position, RHSPosition),
    (
        bitset_is_set(Walls, RHSPosition), !
    ;   bitset_is_set(VisitedNodes, RHSPosition)
    ).

is_best_known_cost(Position, Direction, Cost) :-
    best_cost(Position, Direction, PreviousBestCost)
->  Cost #< PreviousBestCost
;   true.

check_best_cost(Position, Direction, Cost) :-
    is_best_known_cost(Position, Direction, Cost),
    retractall(best_cost(Position, Direction, _)),
    assertz(best_cost(Position, Direction, Cost)).

move_forward(Config, State, State) :- at_end(Config, State), !.

move_forward(Config, StartState, EndState) :-
    state(VisitedNodes, Position, Direction, Cost) = StartState,
    % make sure this is the best cost we've seen so far for this position thus far
    check_best_cost(Position, Direction, Cost),
    % calculate the next position
    config_size(Config, Width-_),
    next_absolute_position(Width, Direction, Position, NextPosition),
    % make sure we haven't visted the next position before
    \+ bitset_is_set(VisitedNodes, NextPosition),
    % make sure the next position isn't a wall
    config_walls(Config, Walls),
    \+ bitset_is_set(Walls, NextPosition),
    % recurse
    bitset_set(VisitedNodes, NextPosition, 1, NextVistedNodes),
    NextCost #= Cost + 1,
    NextState = state(NextVistedNodes, NextPosition, Direction, NextCost),
    % optimization: if we cannot turn, we can recurse directly to move_forward and eliminate the choice points for turning.
    (
        cannot_turn(Config, NextState)
    ->  move_forward(Config, NextState, EndState)
    ;   move_forward_or_turn(Config, NextState, EndState)
    ).

%! next_direction(TurnDirection, StartDirection, EndDirection) is undefined.
turn(left,  up,    left).
turn(left,  left,  down).
turn(left,  down,  right).
turn(left,  right, up).
turn(right, up,    right).
turn(right, left,  up).
turn(right, down,  left).
turn(right, right, down).

turn(TurnDirection, Config, state(VisitedNodes, Position, Direction, Cost), EndState) :-
    turn(TurnDirection, Direction, NextDirection),
    NextCost #= Cost + 1000,
    move_forward(Config, state(VisitedNodes, Position, NextDirection, NextCost), EndState).

best_state(State1, State2, BestState) :-
    state_cost(State1, Cost1),
    state_cost(State2, Cost2),
    (
        Cost1 #< Cost2
    ->  BestState = State1
    ;   BestState = State2
    ).

solve1(Input, EndState) :- init(Input, Config, StartState), !, move_forward_or_turn(Config, StartState, EndState).

:- dynamic(saved_best_state/1).

is_best_state(State) :-
    saved_best_state(PreviousBest)
->  best_state(PreviousBest, State, State)
;   true.

update_best_state(State) :-
    is_best_state(EndState)
->  retractall(saved_best_state(_)),
    assertz(saved_best_state(EndState))
;   true.

solve(Config, StartState, BestCost) :-
    retractall(saved_best_state(_)),
    forall(move_forward_or_turn(Config, StartState, EndState), update_best_state(EndState)),
    saved_best_state(BestState),
    state_cost(BestState, BestCost).

solve(Input, BestCost) :- init(Input, Config, StartState), solve(Config, StartState, BestCost), !.

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

eliminate_dead_ends(Size, StartPosition, EndPosition, Walls0, Walls) :-
    writeln(`Eliminating dead ends...`),
    findall(Position,
            (
                dead_end(Size, Walls0, Position),
                Size = Width-_,
                xy_absolute_position(Width, XYPosition, Position),
                format('Eliminating dead end at ~w~n', [XYPosition]),
                Position #\= StartPosition,
                Position #\= EndPosition
            ),
            DeadEnds),
    (
        DeadEnds = []
    ->  Walls = Walls0
    ;   foldl([Position, WallsIn, WallsOut]>>bitset_set(WallsIn, Position, 1, WallsOut),
              DeadEnds,
              Walls0,
              Walls1),
        eliminate_dead_ends(Size, StartPosition, EndPosition, Walls1, Walls)
    ).

:- dynamic(best_cost/3).

init(Input, config(Walls, Size, StartPosition, EndPosition), state(VisitedNodes, Position, Direction, Cost)) :-
    parse_file(Input, Walls0, Size, StartPosition, EndPosition),
    eliminate_dead_ends(Size, StartPosition, EndPosition, Walls0, Walls),
    bitset_set(0, StartPosition, 1, VisitedNodes),
    Position = StartPosition,
    Direction = right,
    Cost = 0,
    retractall(best_cost(_, _, _)).

at_end(config(_Walls, _Size, _StartPosition, EndPosition), State) :-
    state(_VisitedNodes, Position, _Direction, _Cost) = State,
    Position = EndPosition.

wander(Config, StartState, EndState) :-
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

move_forward(Config, State, State) :- at_end(Config, State).

is_best_known_cost(Position, Direction, Cost) :-
    best_cost(Position, Direction, PreviousBestCost)
->  % format(`New Cost ~w < PreviousBestCost ~w ?~n`, [Cost, PreviousBestCost]),
    Cost #< PreviousBestCost
    % writeln(yes)
;   true.

config_size(config(_, Size, _, _), Size).

store_best_cost(_Config, Position, Direction, Cost) :-
    retractall(best_cost(Position, Direction, _)),
    % config_size(Config, Width-_),
    % xy_absolute_position(Width, XYPosition, Position),
    % format(`New best cost for ~w ~w is ~w~n`, [XYPosition, Direction, Cost]),
    assertz(best_cost(Position, Direction, Cost)).

check_best_cost(Config, Position, Direction, Cost) :-
    is_best_known_cost(Position, Direction, Cost),
    store_best_cost(Config, Position, Direction, Cost).

move_forward(Config, StartState, EndState) :-
    \+ at_end(Config, StartState),
    state(VisitedNodes, Position, Direction, Cost) = StartState,
    % (
    %     Position mod 500 #= 0
    % ->  config_size(Config, Width-_),
    %     xy_absolute_position(Width, XYPosition, Position),
    %     format('Position = ~w, Direction = ~w, Cost = ~w~n', [XYPosition, Direction, Cost])
    % ;   true
    % ),
    check_best_cost(Config, Position, Direction, Cost),
    config(Walls, Width-_Height, _StartPosition, _EndPosition) = Config,
    next_absolute_position(Width, Direction, Position, NextPosition),
    \+ bitset_is_set(VisitedNodes, NextPosition),
    \+ bitset_is_set(Walls, NextPosition),
    bitset_set(VisitedNodes, NextPosition, 1, NextVistedNodes),
    NextCost #= Cost + 1,
    NextState = state(NextVistedNodes, NextPosition, Direction, NextCost),
    !,
    (
        cannot_turn(Config, NextState)
    ->  move_forward(Config, NextState, EndState)
    ;   wander(Config, NextState, EndState)
    ).


%! next_direction(TurnDirection, StartDirection, EndDirection) is undefined.
turn(left, up, left).
turn(left, left, down).
turn(left, down, right).
turn(left, right, up).
turn(right, up, right).
turn(right, left, up).
turn(right, down, left).
turn(right, right, down).

config_end_position(config(_, _, _, EndPosition), EndPosition).

turn(TurnDirection, Config, state(VisitedNodes, Position, Direction, Cost), EndState) :-
    \+ config_end_position(config, Position),
    % config_size(Config, Width-_),
    % xy_absolute_position(Width, XYPosition, Position),
    % format('@ ~w, turn ~w~n', [XYPosition, Direction]),
    turn(TurnDirection, Direction, NextDirection),
    NextCost #= Cost + 1000,
    % check_best_cost(Config, Position, NextDirection, NextCost),
    % store_best_cost(Position, NextDirection, NextCost),
    move_forward(Config, state(VisitedNodes, Position, NextDirection, NextCost), EndState).

best_solution(Solution1, Solution2, BestSolution) :-
    state(_, _, _, Cost1) = Solution1,
    state(_, _, _, Cost2) = Solution2,
    (
        Cost1 #< Cost2
    ->  BestSolution = Solution1
    ;   BestSolution = Solution2
    ).

:- dynamic(saved_best_solution/1).

solve(Config, StartState, BestCost) :-
    retractall(saved_best_solution(_)),
    forall(wander(Config, StartState, EndState),
           (
               saved_best_solution(PreviousBest)
           ->  best_solution(PreviousBest, EndState, NewBest),
               retractall(saved_best_solution(_)),
               assertz(saved_best_solution(NewBest))
           ;   assertz(saved_best_solution(EndState))
           )),
    saved_best_solution(state(_VisitedNodes, _Position, _Direction, BestCost)).

solve(Input, BestCost) :- init(Input, Config, StartState), solve(Config, StartState, BestCost), !.

solve1(Input, EndState) :- init(Input, Config, StartState), !, wander(Config, StartState, EndState).

:- module(bitset_grid_util, [xy_absolute_position/3,
                             next_absolute_position/4,
                             next_xy_position/4,
                             xy_position/2,
                             absolute_position/2]).

:- use_module(library(clpfd)).

%!  absolute_position(Width, X-Y, AbsolutePosition) is semidet.
%
%   AbsolutePosition is the absolute index of coordinate X-Y in a grid of Width, e.g. if Width = 5 then
%   AbsolutePosition = 9 translates to X = 4, Y = 1.
xy_absolute_position(Width, X-Y, AbsolutePosition) :-
    MaxX #= Width - 1,
    X in 0..MaxX,
    Y #>= 0,
    AbsolutePosition #= X + (Y * Width).

%!  next_absolute_position(Width, Direction, Position, NextPosition) is semidet.
%
%   NextPosition is the position next to Position in Direction.
next_absolute_position(Width,  up,    Position, NextPosition) :- NextPosition #= Position - Width.
next_absolute_position(Width,  down,  Position, NextPosition) :- NextPosition #= Position + Width.
next_absolute_position(_Width, left,  Position, NextPosition) :- NextPosition #= Position - 1.
next_absolute_position(_Width, right, Position, NextPosition) :- NextPosition #= Position + 1.

%!  next_xy_position(++Width, ++Direction, ++XYPosition, -NextXYPosition) is semidet.
%!  next_xy_position(++Width, ++Direction, -XYPosition, ++NextXYPosition) is semidet.
%!  next_xy_position(++Width, -Direction, ++XYPosition, ++NextXYPosition) is semidet.
%!  next_xy_position(++Width, -Direction, ++XYPosition, -NextXYPosition) is nondet.
%!  next_xy_position(++Width, -Direction, --XYPosition, ++NextXYPosition) is nondet.
%
%   NextXYPosition is the next X-Y position in Direction from XYPosition in a grid of Width.

next_xy_position(Width, Direction, XYPosition, NextXYPosition) :-
    xy_absolute_position(Width, XYPosition, AbsolutePosition),
    next_absolute_position(Width, Direction, AbsolutePosition, NextAbsolutePosition),
    xy_absolute_position(Width, NextXYPosition, NextAbsolutePosition).

%!  position(Width-Height, X-Y) is nondet.
%
%   X-Y is a position within a grid of size Width x Height.
xy_position(Width-Height, X-Y) :-
    MaxX #= Width - 1,
    MaxY #= Height - 1,
    X in 0..MaxX,
    Y in 0..MaxY.

%!  absolute_position(Width-Height, AbsolutePosition) is nondet.
%
%   AbsolutePosition is an absolute position in a grid of size Width x Height.
absolute_position(Width-Height, AbsolutePosition) :-
    MaxPosition #= (Width * Height) - 1,
    AbsolutePosition in 0..MaxPosition.

%!  grid_forall_positions(Size, Goal) is det.
%
%   Call Goal(AbsolutePosition) for every position in a grid of Size.
grid_forall_positions(Size, Goal) :-
    forall((
               absolute_position(Size, AbsolutePosition),
               label([AbsolutePosition])
           ),
           call(Goal, AbsolutePosition)).

%!  grid_forall_positions(Size, EachPositionGoal, EachLineGoal) is det.
%
%   Call EachPositionGoal(AbsolutePosition) for every position in a grid of Size. If position is the first position in a
%   line, call EachLineGoal(Position) as well.
grid_forall_positions_goal(Width, EachPositionGoal, EachLineGoal, Position) :-
    (
        Position mod Width #= 0
    ->  call(EachLineGoal, Position)
    ;   true
    ),
    call(EachPositionGoal, Position).

grid_forall_positions(Width-Height, EachPositionGoal, EachLineGoal) :-
    grid_forall_positions(Width-Height, call(grid_forall_positions_goal(Width, EachPositionGoal, EachLineGoal))).

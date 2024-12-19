:- module(bitset_grid_util, [xy_absolute_position/3,
                             unchecked_next_absolute_position/4,
                             next_abs_position/4,
                             next_xy_position/3,
                             xy_position/2,
                             absolute_position/2,
                             grid_forall_positions/2,
                             grid_forall_positions/3]).

:- use_module(library(clpfd)).

%!  xy_absolute_position(++Width, ++XYPosition, -AbsolutePosition) is semidet.
%!  xy_absolute_position(++Width, -XYPosition, ++AbsolutePosition) is semidet.
%
%   AbsolutePosition is the absolute index of coordinate X-Y in a grid of Width, e.g. if Width = 5 then
%   AbsolutePosition = 9 translates to X = 4, Y = 1.
xy_absolute_position(Width, X-Y, AbsolutePosition) :-
    X #= AbsolutePosition mod Width,
    Y #= AbsolutePosition // Width,
    AbsolutePosition #= X + (Y * Width).

%!  next_absolute_position(Width, Direction, Position, NextXYPosition) is semidet.
%
%   NextXYPosition is the position next to Position in Direction. Does not check if this position is valid, so only use
%   it on grids that have walls on the edges.
unchecked_next_absolute_position(Width,  up,    Position, NextXYPosition) :- NextXYPosition #= Position - Width.
unchecked_next_absolute_position(Width,  down,  Position, NextXYPosition) :- NextXYPosition #= Position + Width.
unchecked_next_absolute_position(_Width, left,  Position, NextXYPosition) :- NextXYPosition #= Position - 1.
unchecked_next_absolute_position(_Width, right, Position, NextXYPosition) :- NextXYPosition #= Position + 1.

%!  next_abs_position(Size, Direction, Position, NextPosition) is nondet.
%
%   This is checked (only returns valid positions in a given direction).
next_abs_position(Width-_,      left,  P, NextP) :- P mod Width #> 0,         NextP #= P - 1.
next_abs_position(Width-_,      right, P, NextP) :- P mod Width #< Width - 1, NextP #= P + 1.
next_abs_position(Width-_,      up,    P, NextP) :- P // Width #> 0,          NextP #= P - Width.
next_abs_position(Width-Height, down,  P, NextP) :- P // Width #< Height - 1, NextP #= P + Width.

%!  next_xy_position(++Direction, ++XYPosition, -NextXYPosition) is semidet.
%!  next_xy_position(++Direction, -XYPosition, ++NextXYPosition) is semidet.
%!  next_xy_position(-Direction, ++XYPosition, ++NextXYPosition) is semidet.
%!  next_xy_position(-Direction, ++XYPosition, -NextXYPosition) is nondet.
%!  next_xy_position(-Direction, --XYPosition, ++NextXYPosition) is nondet.
%
%   NextXYPosition is the next X-Y position in Direction from XYPosition. Does not enforce that this position is a valid
%   position on the grid (in case you want to wrap over)... use xy_position/2 to do that.

next_xy_position(up,    X-Y0, X-Y) :- Y #= Y0 - 1.
next_xy_position(down,  X-Y0, X-Y) :- Y #= Y0 + 1.
next_xy_position(left,  X0-Y, X-Y) :- X #= X0 - 1.
next_xy_position(right, X0-Y, X-Y) :- X #= X0 + 1.

%!  xy_position(Size, XYPosition) is nondet.
%
%   XYPosition is a position within a grid of Size..
xy_position(Width-Height, X-Y) :-
    MaxX #= Width - 1,
    MaxY #= Height - 1,
    X in 0..MaxX,
    Y in 0..MaxY.

%!  absolute_position(Size, AbsolutePosition) is nondet.
%
%   AbsolutePosition is an absolute position in a grid of size Width x Height.
absolute_position(Width-Height, AbsolutePosition) :-
    MaxPosition #= (Width * Height) - 1,
    AbsolutePosition in 0..MaxPosition.

:- meta_predicate grid_forall_positions(?, 1).

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

:- meta_predicate grid_forall_positions(?, 1, 1).

grid_forall_positions(Width-Height, EachPositionGoal, EachLineGoal) :-
    grid_forall_positions(Width-Height, call(grid_forall_positions_goal(Width, EachPositionGoal, EachLineGoal))).

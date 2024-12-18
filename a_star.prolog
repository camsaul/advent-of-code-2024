:- module(a_star, [a_star/5]).

:- use_module(library(assoc), [put_assoc/4, empty_assoc/1, del_min_assoc/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [reverse/2]).

:- use_module(bitset_grid_util, [xy_absolute_position/3]).
:- use_module(util, [bitset_set/4, bitset_is_set/2]).

:- meta_predicate a_star(?, 2, ?, ?, ?).

% node(XYPos, CostThusFar, EstimatedRemainingCost, Visited)

%!  a_star(Size, NeighborGoal, StartXYPosition, GoalXYPosition, Path) is nondet.
%
%   A* Search algorithm to find the best path from StartXYPosition to GoalXYPosition.
%
%   NeighborGoal(XYPosition, NextXYPosition) is called to find valid neighboring positions from a given position.
a_star(Size, NeighborGoal, StartXY, GoalXY, Path) :-
    estimated_cost_to_node(StartXY, GoalXY, EstimatedCost),
    empty_assoc(OpenSet0),
    StartNode = node(StartXY, 0, EstimatedCost, [StartXY]),
    open_set_add_node(OpenSet0, StartNode, OpenSet),
    closed_set_add_xy(Size, 0, StartXY, ClosedSet),
    search(Size, NeighborGoal, OpenSet, ClosedSet, GoalXY, Path).

node_key(node(XY, CostThusFar, EstRemainingCost, _Visited), Key) :-
    TotalEstCost #= CostThusFar + EstRemainingCost,
    Key = [TotalEstCost, XY].

open_set_add_node(OpenSet0, Node, OpenSet1) :- node_key(Node, Key), put_assoc(Key, OpenSet0, Node, OpenSet1).

open_set_add_nodes(OpenSet, [], OpenSet).
open_set_add_nodes(OpenSet0, [Node|More], OpenSet) :- open_set_add_node(OpenSet0, Node, OpenSet1), open_set_add_nodes(OpenSet1, More, OpenSet).

closed_set_add_xy(Width-_, BitSet0, XY, BitSet) :-
    xy_absolute_position(Width, XY, AbsolutePosition),
    bitset_set(BitSet0, AbsolutePosition, 1, BitSet).

closed_set_xy_is_present(Width-_, BitSet, XY) :-
    xy_absolute_position(Width, XY, AbsolutePosition),
    bitset_is_set(BitSet, AbsolutePosition).

%!  estimated_cost_to_node(CurrentXYPosition, GoalXYPosition, EstimatedCost) is det.
%
%   Estimates the cost from the current node to the goal.
estimated_cost_to_node(X1-Y1, X2-Y2, EstimatedCost) :- EstimatedCost #= abs(X1 - X2) + abs(Y1 - Y2).

%!  cost_to_neighbor(XYPos, NextXYPos, EstimatedCost) is det.
%
%   The cost of moving from one position to an adjacent one..
cost_to_neighbor(_XYPos, _NextXYPost, 1).

% TODO -- use a bitset for visited? Or a tree? Anything better than an ordered set.
% a_star_set_visited(Width, BitSet0, XY, BitSet1) :- xy_absolute_position(Width, XY, Pos), bitset_set(BitSet0, Pos, 1, BitSet1).

:- meta_predicate all_neighbors(?, 2, ?, ?, ?, ?).

all_neighbors(Size, NeighborGoal, ClosedSet, GoalXY, node(XY, CostThusFar, _, Visited), Neighbors) :-
    findall(node(NextXY, NextCostThusFar, NextEstimatedRemainingCost, [NextXY|Visited]),
            (
                call(NeighborGoal, XY, NextXY),
                % \+ memberchk(NextXY, Visited),
                \+ closed_set_xy_is_present(Size, ClosedSet, NextXY),
                cost_to_neighbor(XY, NextXY, CostToNext),
                NextCostThusFar #= CostThusFar + CostToNext,
                estimated_cost_to_node(NextXY, GoalXY, NextEstimatedRemainingCost)
            ),
            Neighbors).

search(Size, NeighborGoal, OpenSet, ClosedSet, GoalXY, Path) :-
    select_best_node(OpenSet, RestOpenSet, BestNode),
    node(XY, _, _, Visited) = BestNode,
    (
        XY = GoalXY
    ->  reverse(Visited, Path)
    ;   all_neighbors(Size, NeighborGoal, ClosedSet, GoalXY, BestNode, Neighbors),
        open_set_add_nodes(RestOpenSet, Neighbors, NewOpenSet),
        closed_set_add_xy(Size, ClosedSet, XY, NewClosedSet),
        search(Size, NeighborGoal, NewOpenSet, NewClosedSet, GoalXY, Path)
    ).

select_best_node(Tree, RestTree, BestNode) :- del_min_assoc(Tree, _Key, BestNode, RestTree).

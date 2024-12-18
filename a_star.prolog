:- module(a_star, [a_star/4]).

:- use_module(library(clpfd)).
:- use_module(library(ordsets), [ord_union/3]).
:- use_module(library(lists), [reverse/2]).

:- use_module(util, [predmin/4]).

:- meta_predicate a_star(2, ?, ?, ?).

% node(XYPos, CostThusFar, EstimatedRemainingCost, Visited)

%!  a_star(NeighborGoal, StartXYPosition, GoalXYPosition, Path) is nondet.
%
%   A* Search algorithm to find the best path from StartXYPosition to GoalXYPosition.
%
%   NeighborGoal(XYPosition, NextXYPosition) is called to find valid neighboring positions from a given position.
a_star(NeighborGoal, StartXY, GoalXY, Path) :-
    estimated_cost_to_node(StartXY, GoalXY, EstimatedCost),
    a_star_search(NeighborGoal, [node(StartXY, 0, EstimatedCost, [StartXY])], [], GoalXY, Path).

%!  estimated_cost_to_node(CurrentXYPosition, GoalXYPosition, EstimatedCost) is det.
%
%   Estimates the cost from the current node to the goal.
estimated_cost_to_node(X1-Y1, X2-Y2, EstimatedCost) :- EstimatedCost #= abs(X1 - X2) + abs(Y1 - Y2).

%!  a_star_cost_to_neighbor(XYPos, NextXYPos, EstimatedCost) is det.
%
%   The cost of moving from one position to an adjacent one..
a_star_cost_to_neighbor(_XYPos, _NextXYPost, 1).

% TODO -- use a bitset for visited? Or a tree? Anything better than an ordered set.
% a_star_set_visited(Width, BitSet0, XY, BitSet1) :- xy_absolute_position(Width, XY, Pos), bitset_set(BitSet0, Pos, 1, BitSet1).

:- meta_predicate a_star_search(2, ?, ?, ?, ?).

a_star_search(NeighborGoal, OpenSet, ClosedSet, GoalXY, Path) :-
    select_best_node(OpenSet, RestOpenSet, node(XY, CostThusFar, _EstimatedRemainingCost, Visited)),
    (
        XY = GoalXY
    ->  reverse(Visited, Path)
    ;   findall(node(NextXY, NextCostThusFar, NextEstimatedRemainingCost, [NextXY|Visited]),
                (
                    call(NeighborGoal, XY, NextXY),
                    \+ memberchk(NextXY, Visited),
                    a_star_cost_to_neighbor(XY, NextXY, CostToNext),
                    NextCostThusFar #= CostThusFar + CostToNext,
                    estimated_cost_to_node(NextXY, GoalXY, NextEstimatedRemainingCost)
                ),
                Neighbors),
        ord_union(ClosedSet, [XY], NewClosedSet),
        ord_union(RestOpenSet, Neighbors, NewOpenSet),
        !,
        a_star_search(NeighborGoal, NewOpenSet, NewClosedSet, GoalXY, Path)
    ).

a_star_estimated_total_cost(node(_XY, CostThusFar, EstimatedRemainingCost, _Visited), EstimatedTotalCost) :-
    EstimatedTotalCost #= CostThusFar + EstimatedRemainingCost.

compare_nodes(Delta, NodeX, NodeY) :-
    a_star_estimated_total_cost(NodeX, CostX),
    a_star_estimated_total_cost(NodeY, CostY),
    (
        CostX > CostY
    ->  Delta = '>'
    ;   CostX < CostY
    ->  Delta = '<'
    ;   Delta = '='
    ).

:- meta_predicate predmin(3, ?, ?, ?).

select_best_node(Nodes, OtherNodes, BestNode) :- predmin(compare_nodes, Nodes, OtherNodes, BestNode).

:- module(a_star, [a_star/4]).

:- use_module(library(assoc), [put_assoc/4, empty_assoc/1, del_min_assoc/4, get_assoc/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [reverse/2]).

:- meta_predicate a_star(2, ?, ?, ?).

% node(XYPos, CostThusFar, EstimatedRemainingCost, Path)
%
% XYPos is a pair of X-Y
% Path is a list of X-Y pairs that have already been visited in reverse order.
%
% OpenSet is an assoc_list (AVL tree) of EstimatedRemainingCost-X-Y => Node for nodes that still need searching.
% ClosedSet is an assoc list of X-Y => 1 of nodes that have already been searched.

%!  a_star(Size, NeighborGoal, StartXYPosition, GoalXYPosition, Path) is nondet.
%
%   A* Search algorithm to find the best path from StartXYPosition to GoalXYPosition in a grid of Size.
%
%   NeighborGoal(XYPosition, NextXYPosition) is called to find valid neighboring positions from a given position; it
%   should succeed once for each valid position.
a_star(NeighborGoal, StartXY, GoalXY, Path) :-
    estimated_cost_to_node(StartXY, GoalXY, EstimatedCost),
    empty_assoc(OpenSet0),
    StartNode = node(StartXY, 0, EstimatedCost, [StartXY]),
    open_set_add_node(OpenSet0, StartNode, OpenSet),
    empty_assoc(ClosedSet0),
    closed_set_add_xy(ClosedSet0, StartXY, ClosedSet),
    search(NeighborGoal, OpenSet, ClosedSet, GoalXY, Path).

node_key(node(XY, CostThusFar, EstRemainingCost, _Path), Key) :-
    TotalEstCost #= CostThusFar + EstRemainingCost,
    Key = [TotalEstCost, XY].

open_set_add_node(OpenSet0, Node, OpenSet1) :- node_key(Node, Key), put_assoc(Key, OpenSet0, Node, OpenSet1).

open_set_add_nodes(OpenSet, [], OpenSet).
open_set_add_nodes(OpenSet0, [Node|More], OpenSet) :- open_set_add_node(OpenSet0, Node, OpenSet1), open_set_add_nodes(OpenSet1, More, OpenSet).

closed_set_add_xy(ClosedSet0, XY, ClosedSet) :- put_assoc(XY, ClosedSet0, 1, ClosedSet).
closed_set_xy_is_present(ClosedSet, XY)      :- get_assoc(XY, ClosedSet, 1).

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

:- meta_predicate all_neighbors(2, ?, ?, ?, ?).

all_neighbors(NeighborGoal, ClosedSet, GoalXY, node(XY, CostThusFar, _, Path), Neighbors) :-
    findall(node(NextXY, NextCostThusFar, NextEstimatedRemainingCost, [NextXY|Path]),
            (
                call(NeighborGoal, XY, NextXY),
                % \+ memberchk(NextXY, Path),
                \+ closed_set_xy_is_present(ClosedSet, NextXY),
                cost_to_neighbor(XY, NextXY, CostToNext),
                NextCostThusFar #= CostThusFar + CostToNext,
                estimated_cost_to_node(NextXY, GoalXY, NextEstimatedRemainingCost)
            ),
            Neighbors).

search(NeighborGoal, OpenSet, ClosedSet, GoalXY, GoalPath) :-
    select_best_node(OpenSet, RestOpenSet, BestNode),
    node(XY, _, _, Path) = BestNode,
    (
        XY = GoalXY
    ->  reverse(Path, GoalPath)
    ;   all_neighbors(NeighborGoal, ClosedSet, GoalXY, BestNode, Neighbors),
        open_set_add_nodes(RestOpenSet, Neighbors, NewOpenSet),
        closed_set_add_xy(ClosedSet, XY, NewClosedSet),
        search(NeighborGoal, NewOpenSet, NewClosedSet, GoalXY, GoalPath)
    ).

select_best_node(Tree, RestTree, BestNode) :- del_min_assoc(Tree, _Key, BestNode, RestTree).

:- module(a_star, [a_star/5]).

:- use_module(library(assoc), [put_assoc/4, empty_assoc/1, del_min_assoc/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [reverse/2]).

:- use_module(bitset_grid_util, [xy_absolute_position/3]).
:- use_module(util, [bitset_set/4, bitset_is_set/2]).

:- meta_predicate a_star(?, 2, ?, ?, ?).

% node(P, CostThusFar, EstimatedRemainingCost, Path)
%
% P is an absolute position (X + (Y * Width))
% Path is a list of absolute positions that have already been visited in reverse order.
%
% OpenSet is an assoc_list (AVL tree) of EstimatedRemainingCost-X-Y => Node for nodes that still need searching.
% ClosedSet is an assoc list of X-Y => 1 of nodes that have already been searched.

%!  a_star(Size, NeighborGoal, StartPosition, GoalPosition, Path) is nondet.
%
%   A* Search algorithm to find the best path from StartPosition to GoalPosition in a grid of Size.
%
%   NeighborGoal(Position, NextPosition) is called to find valid neighboring positions from a given position; it
%   should succeed once for each valid position.
a_star(Size, NeighborGoal, StartPos, GoalPos, Path) :-
    estimated_cost_to_node(Size, StartPos, GoalPos, EstimatedCost),
    empty_assoc(OpenSet0),
    StartNode = node(StartPos, 0, EstimatedCost, [StartPos]),
    open_set_add_node(OpenSet0, StartNode, OpenSet),
    empty_closed_set(ClosedSet0),
    closed_set_add_pos(ClosedSet0, StartPos, ClosedSet),
    search(Size, NeighborGoal, OpenSet, ClosedSet, GoalPos, Path).

node_key(node(P, CostThusFar, EstRemainingCost, _Path), [TotalEstCost, P]) :-
    TotalEstCost #= CostThusFar + EstRemainingCost.

open_set_add_node(OpenSet0, Node, OpenSet1) :- node_key(Node, Key), put_assoc(Key, OpenSet0, Node, OpenSet1).

open_set_add_nodes(OpenSet, [], OpenSet).
open_set_add_nodes(OpenSet0, [Node|More], OpenSet) :- open_set_add_node(OpenSet0, Node, OpenSet1), open_set_add_nodes(OpenSet1, More, OpenSet).

empty_closed_set(0).
closed_set_add_pos(ClosedSet0, P, ClosedSet) :- bitset_set(ClosedSet0, P, 1, ClosedSet).
closed_set_pos_is_present(ClosedSet, P)      :- bitset_is_set(ClosedSet, P).

%!  estimated_cost_to_node(CurrentPosition, GoalPosition, EstimatedCost) is det.
%
%   Estimates the cost from the current node to the goal.
estimated_cost_to_node(Width-_, P1, P2, EstimatedCost) :-
    xy_absolute_position(Width, X1-Y1, P1),
    xy_absolute_position(Width, X2-Y2, P2),
    EstimatedCost #= abs(X1 - X2) + abs(Y1 - Y2).

%!  cost_to_neighbor(P, NextP, EstimatedCost) is det.
%
%   The cost of moving from one position to an adjacent one..
cost_to_neighbor(_P, _NextP, 1).

% TODO -- use a bitset for visited? Or a tree? Anything better than an ordered set.
% a_star_set_visited(Width, BitSet0, P, BitSet1) :- xy_absolute_position(Width, P, Pos), bitset_set(BitSet0, Pos, 1, BitSet1).

:- meta_predicate all_neighbors(?, 2, ?, ?, ?, ?).

all_neighbors(Size, NeighborGoal, ClosedSet, GoalPos, node(P, CostThusFar, _, Path), Neighbors) :-
    findall(node(NextP, NextCostThusFar, NextEstimatedRemainingCost, [NextP|Path]),
            (
                call(NeighborGoal, P, NextP),
                \+ closed_set_pos_is_present(ClosedSet, NextP),
                cost_to_neighbor(P, NextP, CostToNext),
                NextCostThusFar #= CostThusFar + CostToNext,
                estimated_cost_to_node(Size, NextP, GoalPos, NextEstimatedRemainingCost)
            ),
            Neighbors).

search(Size, NeighborGoal, OpenSet, ClosedSet, GoalPos, GoalPath) :-
    select_best_node(OpenSet, RestOpenSet, BestNode),
    node(P, _, _, Path) = BestNode,
    (
        P = GoalPos
    ->  reverse(Path, GoalPath)
    ;   all_neighbors(Size, NeighborGoal, ClosedSet, GoalPos, BestNode, Neighbors),
        open_set_add_nodes(RestOpenSet, Neighbors, NewOpenSet),
        closed_set_add_pos(ClosedSet, P, NewClosedSet),
        search(Size, NeighborGoal, NewOpenSet, NewClosedSet, GoalPos, GoalPath)
    ).

select_best_node(Tree, RestTree, BestNode) :- del_min_assoc(Tree, _Key, BestNode, RestTree).

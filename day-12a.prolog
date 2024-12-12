:- use_module(library(apply), [foldl/4]).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4, get_assoc/3, del_assoc/4, min_assoc/3]).
:- use_module(library(clpfd)).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_chars/5]).

init_cell(Pos, Type, Assoc0, Assoc1) :- put_assoc(Pos, Assoc0, Type, Assoc1).

init(Path, Cells) :-
    empty_assoc(Cells0),
    read_file_lines_to_chars(Path, _Size, init_cell, Cells0, Cells).

neighbor_pos(Row-Col, Row-NextCol, left)  :- NextCol #= Col - 1.
neighbor_pos(Row-Col, Row-NextCol, right) :- NextCol #= Col + 1.
neighbor_pos(Row-Col, NextRow-Col, up)    :- NextRow #= Row - 1.
neighbor_pos(Row-Col, NextRow-Col, down)  :- NextRow #= Row + 1.

neighbors(Pos, Neighbors) :- findall(P, neighbor_pos(Pos, P, _Dir), Neighbors).

plot(Pos, Type, Cells0, Cells, Plot0, Plot) :-
    (
        del_assoc(Pos, Cells0, Type, Cells1),
        Plot1 = [Pos|Plot0]
    )
->  % recurse with neighbor position(s)
    neighbors(Pos, Neighbors),
    foldl({Type}/[NeighborPos, Cells0-Plot0, Cells1-Plot1]>>plot(NeighborPos, Type, Cells0, Cells1, Plot0, Plot1),
          Neighbors, Cells1-Plot1, Cells-Plot)
;   % position not in plot, done recursing.
    Cells = Cells0,
    Plot = Plot0.

plots(Cells, Cells, Plots, Plots) :- empty_assoc(Cells).

plots(Cells0, Cells, Plots0, Plots) :-
    min_assoc(Cells0, Pos, Type),
    plot(Pos, Type, Cells0, Cells1, [], Plot),
    plots(Cells1, Cells, [Type-Plot|Plots0], Plots).

plots(Cells, Plots) :- plots(Cells, _, [], Plots).

edge_pos(Cells, Pos) :- \+ get_assoc(Pos, Cells, _Type).

% "edge" means a neighbor position that belongs to a different plot or off the edge of the board.

cell_edge_pos(Cells, Pos, Type, EdgePos) :-
    neighbor_pos(Pos, EdgePos, _Direction),
    (
        get_assoc(EdgePos, Cells, NeighborType)
    ->  NeighborType \= Type
    ;   edge_pos(Cells, EdgePos)
    ).

num_cell_edges(Cells, Pos, Type, Perimeter) :-
    findall(P, cell_edge_pos(Cells, Pos, Type, P), Neighbors),
    length(Neighbors, Perimeter).

plot_perimeter(Cells, Type-Poss, Perimeter) :-
    foldl({Cells, Type}/[Pos, Acc0, Acc1]>>(num_cell_edges(Cells, Pos, Type, Perimeter), Acc1 #= Acc0 + Perimeter),
          Poss,
          0,
          Perimeter).

plot_area(_Type-Poss, Area) :- length(Poss, Area).

plot_price(Cells, Plot, Price) :-
    plot_perimeter(Cells, Plot, Perimeter),
    plot_area(Plot, Area),
    Price #= Perimeter * Area.

total_price(Cells, Plots, Price) :-
    foldl({Cells}/[Plot, Acc0, Acc1]>>(plot_price(Cells, Plot, Price), Acc1 #= Acc0 + Price), Plots, 0, Price).

input(example, Cells) :- init('day-12-example.txt', Cells).
input(actual, Cells) :- init('day-12.txt', Cells).

solve(Input, Price) :- input(Input, Cells), plots(Cells, Plots), total_price(Cells, Plots, Price).

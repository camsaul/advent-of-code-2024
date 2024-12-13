:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply), [foldl/4]).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4, get_assoc/3, del_assoc/4, max_assoc/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2]).
:- use_module(library(solution_sequences), [group_by/4]).
:- use_module(library(yall)).
:- use_module(util, []).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).

neighbor_pos(Row-Col, Row-NextCol, left)  :- NextCol #= Col - 1.
neighbor_pos(Row-Col, Row-NextCol, right) :- NextCol #= Col + 1.
neighbor_pos(Row-Col, NextRow-Col, up)    :- NextRow #= Row - 1.
neighbor_pos(Row-Col, NextRow-Col, down)  :- NextRow #= Row + 1.

init_cell(Pos, Type, Assoc0, Assoc1) :- put_assoc(Pos, Assoc0, Type, Assoc1).

init(Path, Cells) :-
    empty_assoc(Cells0),
    util:read_file_lines_to_chars(Path, _Size, init_cell, Cells0, Cells).

input(example, Cells) :- init('day-12-example.txt', Cells).
input(actual, Cells) :- init('day-12.txt', Cells).
input(Path, Cells) :- string(Path), init(Path, Cells).

plot(Pos, Type, Cells0, Cells, Plot0, Plot) :-
    (
        del_assoc(Pos, Cells0, Type, Cells1),
        Plot1 = [Pos|Plot0]
    )
->  % recurse with neighbor position(s)
    findall(P, neighbor_pos(Pos, P, _Dir), Neighbors),
    foldl({Type}/[NeighborPos, Cells0-Plot0, Cells1-Plot1]>>plot(NeighborPos, Type, Cells0, Cells1, Plot0, Plot1),
          Neighbors, Cells1-Plot1, Cells-Plot)
;   % position not in plot, done recursing.
    Cells = Cells0,
    Plot = Plot0.

plots(Cells, Cells, Plots, Plots) :- empty_assoc(Cells).

plots(Cells0, Cells, Plots0, Plots) :-
    max_assoc(Cells0, Pos, Type),
    plot(Pos, Type, Cells0, Cells1, [], Plot),
    plots(Cells1, Cells, [Type-Plot|Plots0], Plots).

plots(Cells, Plots) :- plots(Cells, _, [], Plots).

% "edge" means a neighbor position that belongs to a different plot or off the edge of the board.

cell_edge_pos(Cells, Pos, Type, EdgePos) :-
    neighbor_pos(Pos, EdgePos, _Direction),
    (
        get_assoc(EdgePos, Cells, NeighborType)
    ->  NeighborType \= Type
    ;   true % not in the assoc list = an edge position
    ).

cell_num_edges(Cells, Pos, Type, NumEdges) :- aggregate_all(count,  cell_edge_pos(Cells, Pos, Type, _P), NumEdges).

plot_perimeter_len(Cells, Type-Positions, Perimeter) :-
    aggregate_all(sum(NumEdges), (member(Pos, Positions), cell_num_edges(Cells, Pos, Type, NumEdges)), Perimeter).

plot_area(_Type-Positions, Area) :- length(Positions, Area).

:- discontiguous(plot_price/4).

plot_price(part1, Cells, Plot, Price) :-
    plot_perimeter_len(Cells, Plot, PerimeterLength),
    plot_area(Plot, Area),
    Price #= PerimeterLength * Area.

total_price(PriceType, Cells, Plots, Price) :-
    aggregate_all(sum(Price0), (member(Plot, Plots), plot_price(PriceType, Cells, Plot, Price0)), Price).

% solve(actual, part1, Price).
solve(Input, PriceType, Price) :- input(Input, Cells), plots(Cells, Plots), total_price(PriceType, Cells, Plots, Price), !.

%
%
% Part 2
%
%

edge(R1-C, R2-C, edge{dir:h, between:[R1, R2], start:C}).
edge(R-C1, R-C2, edge{dir:v, between:[C1, C2], start:R}).

cell_edge(Cells, P1, Type, Edge) :- cell_edge_pos(Cells, P1, Type, P2), edge(P1, P2, Edge).

edges(Cells, Type-Positions, Edges) :-
    util:mapcat({Cells, Type}/[P1, Edges]>>findall(Edge, cell_edge(Cells, P1, Type, Edge), Edges),
                Positions,
                Edges).

edge_groups(Edges, Groups) :-
    findall(Group,
            group_by([Dir, Between],
                     Start,
                     (member(Edge, Edges), Dir = Edge.dir, Between = Edge.between, Start = Edge.start),
                     Group),
            Groups).

reduce_sections([], Acc, Acc).
reduce_sections([Edge|More], [], Acc) :- reduce_sections(More, [[Edge]], Acc).

reduce_sections([Edge|MoreEdges], [Section|MoreSections], Acc) :-
    [LastEdge|_] = Section,
    (
        Edge #= LastEdge + 1
    ->  Acc0 = [[Edge|Section]|MoreSections]
    ;   Acc0 = [[Edge],Section|MoreSections]
    ),
    reduce_sections(MoreEdges, Acc0, Acc).

contiguous_sections(Edges, Sections) :- sort(Edges, Sorted), reduce_sections(Sorted, [], Sections).

plot_num_sides(Cells, Plot, NumSides) :-
    edges(Cells, Plot, Edges),
    edge_groups(Edges, Groups),
    util:mapcat(contiguous_sections, Groups, Sides),
    length(Sides, NumSides).

plot_price(part2, Cells, Plot, Price) :-
    plot_num_sides(Cells, Plot, NumSides),
    plot_area(Plot, Area),
    Price #= NumSides * Area.

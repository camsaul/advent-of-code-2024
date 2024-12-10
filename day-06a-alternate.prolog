:- use_module(library(apply), [maplist/2
:- use_module(library(clpfd)).
:- use_module(library(ordsets)).
:- use_module(library(yall)).
:- use_module(util, [read_file_lines_to_chars/2]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(answer_write_options,[max_depth(10)]).

cell_glyph(blocked, '#').
cell_glyph(visited, 'X').
cell_glyph(unvisited, '.').

direction_glyph(up, '^').
direction_glyph(down, 'v').
direction_glyph(left, '<').
direction_glyph(right, '>').

rotate_direction(up, right).
rotate_direction(right, down).
rotate_direction(down, left).
rotate_direction(left, up).

:- dynamic board_size/1.

% P = Position (RowNum-ColNum tuple)
% B = Blocked (set of blocked positions)
% H = History (list of Position-Direction tuples in order from most recent to oldest position-direction)
% D = Direction

blocked(P, B) :- ord_memberchk(P, B).

visited(_P, []) :- false.
visited(P, [P-_ | _MoreHistory]) :- true, !.
visited(P, [_ | MoreHistory]) :- visited(P, MoreHistory).

visited_positions([], []).
visited_positions([P-_ | MoreHistory], [P | MoreVisited]) :- visited_positions(MoreHistory, MoreVisited).

visited_set(H, Set) :-
    visited_positions(H, Visited),
    list_to_ord_set(Visited, Set).

cell_status(P, B, _H, blocked) :- blocked(P, B).
cell_status(P, _B, H, visited) :- visited(P, H).

cell_status(P, B, H, unvisited) :- \+ blocked(P, B), \+ visited(P, H).

next_position(0-_ColNum, up, exit).
next_position(RowNum-ColNum, up, NextRowNum-ColNum) :- RowNum #> 0, NextRowNum #= RowNum - 1.

next_position(RowNum-_ColNum, down, exit) :- board_size(NumRows-_), RowNum #= NumRows - 1.
next_position(RowNum-ColNum, down, NextRowNum-ColNum) :- board_size(NumRows-_), RowNum #< NumRows - 1, NextRowNum #= RowNum + 1.

next_position(_RowNum-0, left, exit).
next_position(RowNum-ColNum, left, RowNum-NextColNum) :- ColNum #> 0, NextColNum #= ColNum - 1.

next_position(_RowNum-ColNum, right, exit) :- board_size(_-NumCols), ColNum #= NumCols - 1.
next_position(RowNum-ColNum, right, RowNum-NextColNum) :- board_size(_-NumCols), ColNum #< NumCols - 1, NextColNum #= ColNum + 1.

exit(_B, H1, H2, Status) :-
    [P1-D1 | _More] = H1,
    next_position(P1, D1, P2),
    !,
    P2 = exit,
    H2 = H1,
    Status = exited.

move(B, H1, H2, Status) :-
    [P1-D1 | _More] = H1,
    next_position(P1, D1, P2),
    !,
    \+ blocked(P2, B),
    D2 = D1,
    H2 = [ P2-D2 | H1 ],
    Status = moved,
    format("Moved ~w from ~w to ~w~n", [D1, P1, P2]).

rotate(_B, H1, H2, Status) :-
    [P1-D1 | _More] = H1,
    rotate_direction(D1, D2),
    H2 = [ P1-D2 | H1 ],
    Status = rotated.

step(B, H1, H2, Status) :-
    exit(B, H1, H2, Status), !
    ;   move(B, H1, H2, Status), !
    ;   rotate(B, H1, H2, Status), !.

iterate(B, H1, H2, Status) :-
    step(B, H1, HNext, NextStatus),
    (
        H2 = HNext,
        Status = NextStatus
    ;   NextStatus \= exited,
        iterate(B, HNext, H2, Status)
    ).

solve(B, H1, H2) :- iterate(B, H1, H2, exited).

print_cell(B, H, Row-Col) :-
    [P-D | _More] = H,
    (
        Row-Col = P
    ->  direction_glyph(D, Glyph)
    ;   cell_status(Row-Col, B, H, Status),
        cell_glyph(Status, Glyph)
    ),
    !,
    write(Glyph).

print_row(NumCols, B, H, Row-Col) :-
    print_cell(B, H, Row-Col),
    (
        Col #= NumCols - 1
    ->  writeln('')
    ;   NextCol #= Col + 1,
        print_row(NumCols, B, H, Row-NextCol)
    ).

print_rows(NumRows-NumCols, B, H, Row) :-
    print_row(NumCols, B, H, Row-0),
    (
        Row #= NumRows - 1
    ->  true
    ;   NextRow #= Row + 1,
        print_rows(NumRows-NumCols, B, H, NextRow)
    ).

print_board(B, H) :-
    board_size(BoardSize),
    print_rows(BoardSize, B, H, 0),
    visited_set(H, Visited),
    length(Visited, NumVisited),
    format("~nVisted so far: ~w~n", [NumVisited]).

board_size(Input, NumRows-NumCols) :-
    length(Input, NumRows),
    maplist({NumCols}/[Row]>>length(Row, NumCols), Input).

init_guard_position_in_row([], _ColNum, _D) :- fail.

init_guard_position_in_row([X | _More], 0, D) :- direction_glyph(D, X).

init_guard_position_in_row([X | More], ColNum, D) :-
    \+ direction_glyph(D, X),
    ColNum #= NextColNum + 1,
    init_guard_position_in_row(More, NextColNum, D).

init_guard_position([], _Position, _D) :- fail.

init_guard_position([Row|More], RowNum-ColNum, D) :-
    RowNum #= 0,
    init_guard_position_in_row(Row, ColNum, D)
;   RowNum #= NextRowNum + 1,
    init_guard_position(More, NextRowNum-ColNum, D).

init_blocked_cells_in_row([], _RowNum, _ColNum, B, B).

init_blocked_cells_in_row([Cell | More], RowNum, ColNum, B1, B2) :-
    (
        cell_glyph(blocked, Cell)
    ->  format("Blocked cell at ~w~n", RowNum-ColNum),
        ord_add_element(B1, RowNum-ColNum, BNext)
    ;   BNext = B1
    ),
    NextColNum #= ColNum + 1,
    init_blocked_cells_in_row(More, RowNum, NextColNum, BNext, B2).

init_blocked_cells([], _RowNum, B, B).

init_blocked_cells([Row | More], RowNum, B1, B2) :-
    init_blocked_cells_in_row(Row, RowNum, 0, B1, BNext),
    NextRowNum #= RowNum + 1,
    init_blocked_cells(More, NextRowNum, BNext, B2).

init_blocked_cells(Input, B) :-
    list_to_ord_set([], B1),
    init_blocked_cells(Input, 0, B1, B).

init(Input, B, H1) :-
    abolish(board_size/1),
    board_size(Input, BoardSize),
    assertz(board_size(BoardSize)),
    format("Board size is ~w~n", [BoardSize]),
    init_guard_position(Input, P1, D1),
    format("Guard is starting at ~w, facing ~w~n", [P1, D1]),
    H1 = [P1-D1],
    init_blocked_cells(Input, B).

init_from_file(Path, B, H1) :-
    read_file_lines_to_chars(Path, Input),
    init(Input, B, H1).

iterate_example(H2, Status) :-
    init_from_file('day-06-example.txt', B, H1),
    iterate(B, H1, H2, Status),
    print_board(B, H2).

solve_example(NumVisited) :-
    init_from_file('day-06-example.txt', B, H1),
    solve(B, H1, H2),
    visited_set(H2, Visited),
    length(Visited, NumVisited).

solve(NumVisited) :-
    init_from_file('day-06.txt', B, H1),
    solve(B, H1, H2),
    visited_set(H2, Visited),
    length(Visited, NumVisited).

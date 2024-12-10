:- use_module(library(apply), [maplist/2, maplist/3, exclude/3]).
:- use_module(library(bounds), [label/1]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2]).
:- use_module(library(ordsets)).
:- use_module(library(statistics), [time/1]).
:- use_module(library(thread)).
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

visited_position_direction(PD, H) :- member(PD, H).

init_history(PD, [PD]).
push_history(H1, PD, [PD|H1]).

current_position_direction([P-D|_More], P, D).

next_position(0-_ColNum, up, exit).
next_position(RowNum-ColNum, up, NextRowNum-ColNum) :- RowNum #> 0, NextRowNum #= RowNum - 1.

next_position(RowNum-_ColNum, down, exit) :- board_size(NumRows-_), RowNum #= NumRows - 1.
next_position(RowNum-ColNum, down, NextRowNum-ColNum) :- board_size(NumRows-_), RowNum #< NumRows - 1, NextRowNum #= RowNum + 1.

next_position(_RowNum-0, left, exit).
next_position(RowNum-ColNum, left, RowNum-NextColNum) :- ColNum #> 0, NextColNum #= ColNum - 1.

next_position(_RowNum-ColNum, right, exit) :- board_size(_-NumCols), ColNum #= NumCols - 1.
next_position(RowNum-ColNum, right, RowNum-NextColNum) :- board_size(_-NumCols), ColNum #< NumCols - 1, NextColNum #= ColNum + 1.

exit(_B, H1, H2, Status) :-
    current_position_direction(H1, P1, D1),
    next_position(P1, D1, P2),
    !,
    P2 = exit,
    % format("Exit at ~w ~w~n", [P1, D1]),
    H2 = H1,
    Status = exited.

move(B, H1, H2, Status) :-
    current_position_direction(H1, P1, D1),
    next_position(P1, D1, P2),
    !,
    \+ blocked(P2, B),
    (
        visited_position_direction(P2-D1, H1)
    ->  H2 = H1,
        % format("Cycle found at ~w ~w~n", [P1, D1]),
        Status = cycle
    ;   push_history(H1, P2-D1, H2),
        Status = moved
        % format("Moved ~w from ~w to ~w~n", [D1, P1, P2])
    ).

rotate(_B, H1, H2, Status) :-
    current_position_direction(H1, P1, D1),
    rotate_direction(D1, D2),
    push_history(H1, P1-D2, H2),
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
    ;   \+ member(NextStatus, [cycle, exited]),
        iterate(B, HNext, H2, Status)
    ).
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
    ->  ord_add_element(B1, RowNum-ColNum, BNext)
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
    init_history(P1-D1, H1),
    init_blocked_cells(Input, B).

init_from_file(Path, B, H1) :-
    read_file_lines_to_chars(Path, Input),
    init(Input, B, H1).

find_one_cycle(B, H1, Cell, Out) :-
    ord_memberchk(Cell, B)
->  Out = false
;   (
        ord_add_element(B, Cell, B2),
        % format("Added blocked cell at ~w~n", Cell),
        (
            iterate(B2, H1, _H2, cycle)
        ->  format("Cycle found when adding block at ~w~n", [Cell]),
            Out = Cell
        ;   Out = false
        )
    ).

find_all_cycles_parallel(B, H1, Out) :-
    board_size(NumRows-NumCols),
    MaxBlockedRow #= NumRows - 1,
    BlockedRow in 0..MaxBlockedRow,
    MaxBlockedCol #= NumCols - 1,
    BlockedCol in 0..MaxBlockedCol,
    findall(BlockedRow-BlockedCol, label([BlockedRow, BlockedCol]), Cells),
    concurrent_maplist(call(find_one_cycle(B, H1)), Cells, Answers),
    time(exclude([X]>>(X = false), Answers, Out)).

solve_example(N) :-
    init_from_file('day-06-example.txt', B, H1),
    find_all_cycles_parallel(B, H1, Cells),
    length(Cells, N).

solve(N) :-
    init_from_file('day-06.txt', B, H1),
    find_all_cycles_parallel(B, H1, Cells),
    length(Cells, N).

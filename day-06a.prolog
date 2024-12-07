s:- use_module(library(clpfd)).

:- use_module(util, [read_file_lines_to_chars/2]).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(lists), [nth0/3, same_length/2, append/3]).
:- use_module(library(yall)).
:- use_module(library(aggregate), [aggregate_all/3]).

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
:- dynamic visited/1.
:- dynamic blocked/1.

cell_status(Position, blocked) :- blocked(Position).
cell_status(Position, visited) :- visited(Position).
cell_status(Position, unvisited) :- \+ blocked(Position), \+ visited(Position).

next_position(0-_ColNum, up, exit).
next_position(RowNum-ColNum, up, NextRowNum-ColNum) :- RowNum #> 0, NextRowNum #= RowNum - 1.

next_position(RowNum-_ColNum, down, exit) :- board_size(NumRows-_), RowNum #= NumRows - 1.
next_position(RowNum-ColNum, down, NextRowNum-ColNum) :- board_size(NumRows-_), RowNum #< NumRows - 1, NextRowNum #= RowNum + 1.

next_position(_RowNum-0, left, exit).
next_position(RowNum-ColNum, left, RowNum-NextColNum) :- ColNum #> 0, NextColNum #= ColNum - 1.

next_position(_RowNum-ColNum, right, exit) :- board_size(_-NumCols), ColNum #= NumCols - 1.
next_position(RowNum-ColNum, right, RowNum-NextColNum) :- board_size(_-NumCols), ColNum #< NumCols - 1, NextColNum #= ColNum + 1.

move(NextPosition) :-
    \+ blocked(NextPosition),
    (
        visited(NextPosition)
    ;   assertz(visited(NextPosition))
    ).

step(Position, Direction, NextPosition, NextDirection, Status) :-
    next_position(Position, Direction, P2),
    !,
    (
        P2 = exit
    ->  Status = exited
    ;   (
            move(P2)
        ->  NextDirection = Direction,
            NextPosition = P2,
            Status = moved,
            format("Moved ~w from ~w to ~w~n", [Direction, Position, NextPosition])
        ;   rotate_direction(Direction, NextDirection),
            NextPosition = Position,
            Status = rotated
        )
    ).

iterate(StartPosition, StartDirection, EndPosition, EndDirection, EndStatus) :-
    step(StartPosition, StartDirection, NextPosition, NextDirection, NextStatus),
    (
        EndPosition = NextPosition,
        EndDirection = NextDirection,
        EndStatus = NextStatus
    ;   NextStatus \= exited,
        iterate(NextPosition, NextDirection, EndPosition, EndDirection, EndStatus)
    ).

solve(InitialPosition, InitialDirection) :- iterate(InitialPosition, InitialDirection, _P2, _D2,  exited).

print_cell(GuardPosition, Direction, Row-Col) :-
    (
        Row-Col = GuardPosition
    ->  direction_glyph(Direction, Glyph)
    ;   cell_status(Row-Col, Status),
        cell_glyph(Status, Glyph)
    ),
    !,
    write(Glyph).

print_row(NumCols, GuardPosition, Direction, Row-Col) :-
    print_cell(GuardPosition, Direction, Row-Col),
    (
        Col #= NumCols - 1
    ->  writeln('')
    ;   NextCol #= Col + 1,
        print_row(NumCols, GuardPosition, Direction, Row-NextCol)
    ).

print_rows(NumRows-NumCols, GuardPosition, Direction, Row) :-
    print_row(NumCols, GuardPosition, Direction, Row-0),
    (
        Row #= NumRows - 1
    ->  true
    ;   NextRow #= Row + 1,
        print_rows(NumRows-NumCols, GuardPosition, Direction, NextRow)
    ).

print_board(GuardPosition, Direction) :-
    board_size(BoardSize),
    print_rows(BoardSize, GuardPosition, Direction, 0),
    aggregate_all(count, visited(_), NumVisited),
    format("~nVisted so far: ~w~n", [NumVisited]).

board_size(Input, NumRows-NumCols) :-
    length(Input, NumRows),
    maplist({NumCols}/[Row]>>length(Row, NumCols), Input).

init_guard_position_in_row([], _ColNum, _Direction) :- fail.

init_guard_position_in_row([X | _More], 0, Direction) :- direction_glyph(Direction, X).

init_guard_position_in_row([X | More], ColNum, Direction) :-
    \+ direction_glyph(Direction, X),
    ColNum #= NextColNum + 1,
    init_guard_position_in_row(More, NextColNum, Direction).

init_guard_position([], _Position, _Direction) :- fail.

init_guard_position([Row|More], RowNum-ColNum, Direction) :-
    RowNum #= 0,
    init_guard_position_in_row(Row, ColNum, Direction)
;   RowNum #= NextRowNum + 1,
    init_guard_position(More, NextRowNum-ColNum, Direction).

init_blocked_cells_in_row([], _RowNum, _ColNum).

init_blocked_cells_in_row([Cell | More], RowNum, ColNum) :-
    (
        cell_glyph(blocked, Cell)
    ->  format("Blocked cell at ~w~n", RowNum-ColNum),
        assertz(blocked(RowNum-ColNum))
    ;   true
    ),
    NextColNum #= ColNum + 1,
    init_blocked_cells_in_row(More, RowNum, NextColNum).

init_blocked_cells([], _RowNum).

init_blocked_cells([Row | More], RowNum) :-
    init_blocked_cells_in_row(Row, RowNum, 0),
    NextRowNum #= RowNum + 1,
    init_blocked_cells(More, NextRowNum).

init_blocked_cells(Input) :-
    abolish(blocked/1),
    init_blocked_cells(Input, 0).

init(Input, InitialPosition, InitialDirection) :-
    abolish(board_size/1),
    abolish(visited/1),
    board_size(Input, BoardSize),
    assertz(board_size(BoardSize)),
    format("Board size is ~w~n", [BoardSize]),
    init_guard_position(Input, InitialPosition, InitialDirection),
    format("Guard is starting at ~w, facing ~w~n", [InitialPosition, InitialDirection]),
    assertz(visited(InitialPosition)),
    init_blocked_cells(Input).

init_from_file(Path, InitialPosition, InitialDirection) :-
    read_file_lines_to_chars(Path, Input),
    init(Input, InitialPosition, InitialDirection).

iterate_example(P1, D1, P2, D2, Status) :-
    init_from_file('day-06-example.txt', P1, D1), iterate(P1, D1, P2, D2, Status), print_board(P2, D2).

num_visited(NumVisited) :-
    aggregate_all(count, visited(_), NumVisited).

solve_example(NumVisited) :-
    init_from_file('day-06-example.txt', P1, D1),
    solve(P1, D1),
    num_visited(NumVisited).

iterate_actual(P1, D1, P2, D2, Status) :-
    init_from_file('day-06.txt', P1, D1), iterate(P1, D1, P2, D2, Status), print_board(P2, D2).

solve(NumVisited) :-
    init_from_file('day-06.txt', P1, D1),
    solve(P1, D1),
    num_visited(NumVisited).

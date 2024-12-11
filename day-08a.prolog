:- use_module(library(apply), [include/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2]).
:- use_module(library(yall), [(>>)/3]).

:- use_module(util, [read_file_lines_to_codes/4]).

:- set_prolog_flag(answer_write_options,[max_depth(15)]).

position(NumRows-NumCols, Row-Col) :-
    Row #>= 0,
    Row #< NumRows,
    Col #>= 0,
    Col #< NumCols.

next_position(Size, R1-C1, R2-C2, RDelta-CDelta) :-
    position(Size, R2-C2),
    RDelta #= R2 - R1,
    CDelta #= C2 - C1,
    (
        R2 #\= R1
    ->  true
    ;   C2 #\= C1
    ).

possible_antinode_location(Size, R-C, A1R-A1C, A2R-A2C) :-
    next_position(Size, R-C, A1R-A1C, RDelta1-CDelta1),
    RDelta2 #= RDelta1 * 2,
    CDelta2 #= CDelta1 * 2,
    next_position(Size, R-C, A2R-A2C, RDelta2-CDelta2).

antenna_location(Antennas, A1, AType) :- member(A1-AType, Antennas).

antinode_location(Antennas, Size, P, A1, A2, AType) :-
    position(Size, P),
    possible_antinode_location(Size, P, A1, A2),
    antenna_location(Antennas, A1, AType),
    antenna_location(Antennas, A2, AType),
    R-C = P,
    A1R-A1C = A1,
    A2R-A2C = A2,
    label([R, C, A1R, A1C, A2R, A2C]).

antinode_locations(Antennas, Size, Out) :-
    findall(R-C, (position(Size, R-C), label([R, C])), Positions),
    include({Antennas, Size}/[P]>>antinode_location(Antennas, Size, P, _A1, _A2, _AType), Positions, Out).

init_antenna(P, Code, P-Code) :- Code \= 0'..

init(Path, Antennas, Size) :-
    read_file_lines_to_codes(Path, Size, init_antenna, Antennas).

example(P, A1, A2, AType) :-
    init('day-08-example.txt', Antennas, Size),
    antinode_location(Antennas, Size, P, A1, A2, AType).

example(Out, N) :-
    init('day-08-example.txt', Antennas, Size),
    antinode_locations(Antennas, Size, Out),
    length(Out, N).

run(N) :-
    init('day-08.txt', Antennas, Size),
    antinode_locations(Antennas, Size, Out),
    length(Out, N).

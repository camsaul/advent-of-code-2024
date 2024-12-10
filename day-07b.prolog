:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_strings/2]).

equation(0, [], []).
equation(Result, [Result], []).

equation(Result, [X, Y], [add]) :- Result #= X + Y.
equation(Result, [X, Y], [multiply]) :- Result #= X * Y.

equation(Result, [X, Y], [concat]) :-
    number_string(X, XStr),
    number_string(Y, YStr),
    string_concat(XStr, YStr, ResultStr),
    number_string(Result, ResultStr).

equation(Result, [X, Y, Z | More], [XYOperator | MoreOperators]) :-
    equation(XYResult, [X, Y], [XYOperator]),
    equation(Result, [XYResult, Z | More], MoreOperators).

parse_line(Line, [Result, Nums]) :-
    split_string(Line, ":", " ", [ResultStr, RestStr]),
    number_string(Result, ResultStr),
    split_string(RestStr, " ", "", NumStrs),
    maplist([S,N]>>number_string(N,S), NumStrs, Nums).

solve(Path, Out) :-
    read_file_lines_to_strings(Path, Lines),
    maplist(parse_line, Lines, Equations),
    foldl([[Result, Nums], Acc1, Acc2]>>(equation(Result, Nums, _) -> Acc2 #= Acc1 + Result ; Acc2 = Acc1), Equations, 0, Out).

solve_example(Out) :- solve('day-07-example.txt', Out).
solve(Out) :- solve('day-07.txt', Out).

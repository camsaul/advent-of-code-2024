% Tested with SWI Prolog.
%
%     $ swipl
%
%     ?- consult('day-01.prolog'), run.
%     11
%     true.
%
%     run also has a 1-arity to specify input different than the example.

split_lines(String, Lines) :-
    split_string(String, "\n", "\n", Lines).

split_line(Line, Parts) :-
    split_string(Line, "\s+", "\s+", Parts).

parse_line(Line, [X, Y]) :-
    split_line(Line, [XString, YString | _]),
    number_string(X, XString),
    number_string(Y, YString).

parse_lines([], [], []) :-
    true.

parse_lines([Line | MoreLines], [List1H | List1T], [List2H | List2T]) :-
    parse_line(Line, [List1H, List2H]),
    parse_lines(MoreLines, List1T, List2T).

parse_input(Input, List1, List2) :-
    split_lines(Input, Lines),
    parse_lines(Lines, List1Unsorted, List2Unsorted),
    msort(List1Unsorted, List1),
    msort(List2Unsorted, List2).

distance(X, Y, Distance) :-
    Distance is abs(X - Y).

distances([], [], []) :-
    true.

distances([List1H | List1T], [List2H | List2T], [DistancesH | DistancesT]) :-
    distance(List1H, List2H, DistancesH),
    distances(List1T, List2T, DistancesT).

reduce_sum([], 0).

reduce_sum([X, Y | More], N) :-
    SumXY is X + Y,
    reduce_sum(More, SumMore),
    N is SumXY + SumMore.

input(
"3   4
4   3
2   5
1   3
3   9
3   3").

run(Input) :-
    parse_input(Input, List1, List2),
    distances(List1, List2, Distances),
    reduce_sum(Distances, TotalDistance),
    writeln(TotalDistance).

% To use the example input,
run() :-
    input(Input),
    run(Input).

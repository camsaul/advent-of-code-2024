:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2]).
:- use_module(library(pcre), [re_matchsub/4]).

:- use_module(util, [read_file_lines_to_strings/2]).

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(re_compile, true).

prize(machine{a: AX-AY, b: BX-BY, prize: PrizeX-PrizeY}, NumTokens) :-
    NumTokens #>= 0,
    NumAPresses #>= 0,
    NumBPresses #>= 0,
    ACost #= NumAPresses * 3,
    BCost #= NumBPresses,
    NumTokens #= ACost + BCost,
    PrizeX - (NumAPresses * AX) - (NumBPresses * BX) #= 0,
    PrizeY - (NumAPresses * AY) - (NumBPresses * BY) #= 0.

parse_button(Line, X-Y) :-
    re_matchsub("Button [AB]: X\\+(\\d+), Y\\+(\\d+)", Line, Matches, []),
    number_string(X, Matches.1),
    number_string(Y, Matches.2).

parse_prize(Part, Line, X-Y) :-
    re_matchsub("Prize: X=(\\d+), Y=(\\d+)", Line, Matches, []),
    number_string(X0, Matches.1),
    number_string(Y0, Matches.2),
    (
        Part = part1, X #= X0, Y #= Y0
    ;   Part = part2, X #= X0 + 10^13, Y #= Y0 + 10^13
    ).

parse_machines(_Part, [], []).

parse_machines(Part, [ALine, BLine, PrizeLine], [machine{a: AXY, b: BXY, prize: PrizeXY}]) :-
    parse_button(ALine, AXY),
    parse_button(BLine, BXY),
    parse_prize(Part, PrizeLine, PrizeXY).

parse_machines(Part, [ALine, BLine, PrizeLine, _BlankLine | More], [Machine|MoreMachines]) :-
    parse_machines(Part, [ALine, BLine, PrizeLine], [Machine]),
    parse_machines(Part, More, MoreMachines).

path(example, 'day-13-example.txt').
path(actual, 'day-13.txt').

parse(Input, Part, Machines) :-
    path(Input, Path),
    read_file_lines_to_strings(Path, Strs),
    parse_machines(Part, Strs, Machines).

% solve(actual, part1, N)
solve(Input, Part, N) :-
    parse(Input, Part, Machines),
    aggregate_all(sum(NumTokens), (member(Machine, Machines), prize(Machine, NumTokens)), N).

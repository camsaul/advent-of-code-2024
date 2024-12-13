:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2]).
:- use_module(library(pcre), [re_matchsub/4]).

:- use_module(util, [read_file_lines_to_strings/2]).

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(re_compile, true).

prize(machine{a: AX-AY, b: BX-BY, prize: PrizeX-PrizeY}, NumAPresses, NumBPresses, NumTokens) :-
    NumTokens #>= 0,
    NumAPresses #>= 0,
    NumBPresses #>= 0,
    ACost #= NumAPresses * 3,
    BCost #= NumBPresses,
    NumTokens #= ACost + BCost,
    PrizeX - (NumAPresses * AX) - (NumBPresses * BX) #= 0,
    PrizeY - (NumAPresses * AY) - (NumBPresses * BY) #= 0.

parse_button(Line, X-Y) :-
    re_matchsub("Button [A|B]: X\\+(\\d+), Y\\+(\\d+)", Line, Matches, []),
    number_string(X, Matches.1),
    number_string(Y, Matches.2).

parse_prize(Line, X-Y) :-
    re_matchsub("Prize: X=(\\d+), Y=(\\d+)", Line, Matches, []),
    number_string(X, Matches.1),
    number_string(Y, Matches.2).

parse_machines([], []).

parse_machines([ALine, BLine, PrizeLine], [machine{a: AXY, b: BXY, prize: PrizeXY}]) :-
    parse_button(ALine, AXY),
    parse_button(BLine, BXY),
    parse_prize(PrizeLine, PrizeXY).

parse_machines([ALine, BLine, PrizeLine, _BlankLine | More], [Machine|MoreMachines]) :-
    parse_machines([ALine, BLine, PrizeLine], [Machine]),
    parse_machines(More, MoreMachines).

path(example, 'day-13-example.txt').
path(actual, 'day-13.txt').

parse(Input, Machines) :- path(Input, Path), read_file_lines_to_strings(Path, Strs), parse_machines(Strs, Machines).

% solve(actual, N)
solve(Input, N) :-
    parse(Input, Machines),
    aggregate_all(sum(NumTokens),
                  (
                      member(Machine, Machines),
                      prize(Machine, _NumA, _NumB, NumTokens)
                  ),
                  N).

:- use_module(library(apply), [exclude/3, foldl/4, maplist/2, maplist/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [nth0/3]).
:- use_module(library(pcre)).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(yall)).

:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).

rbefore(X, X) :- fail, !.
rbefore(X, Y) :- X #\= Y, before(X, Y), !.
rbefore(X, Y) :- X #\= Y, before(Y, X), !, fail.
rbefore(X, Y) :- X #\= Y, before(X, Z), rbefore(Z, Y).

correct_first_element([]).
correct_first_element([_X]).

correct_first_element([X, Y | More]) :-
    \+ rbefore(Y, X),
    correct_first_element([X | More]).

sorted_list([]).
sorted_list([_X]).

sorted_list([X, Y | More]) :-
    rbefore(X, Y),
    % Make sure there are no items in More that should be sorted before X.
    correct_first_element([X | More]),
    sorted_list([Y | More]).

sorted_lists(Lists, SortedLists) :-
    exclude([L]>>(\+ sorted_list(L)), Lists, SortedLists).

middle_element(List, MiddleElement) :-
    length(List, Length),
    Index #= Length // 2,
    nth0(Index, List, MiddleElement).

sum_middle_elements(Lists, Sum) :-
    foldl([L, Acc, NewAcc]>>(middle_element(L, MiddleElement), NewAcc #= Acc + MiddleElement), Lists, 0, Sum).

run(Lists, Sum) :-
    sorted_lists(Lists, SortedLists),
    sum_middle_elements(SortedLists, Sum).

read_stream_lines(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines(Stream, [Line | MoreLines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_stream_lines(Stream, MoreLines).

read_file_lines(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines(Stream, Lines),
    close(Stream).

partition_lines([], _Mode, [], []).

partition_lines([Line | More], rules, Rules, Lists) :-
    string_length(Line, LineLength),
    (
        LineLength #= 0
    ->  partition_lines(More, lists, Rules, Lists)
    ;   (
            Rules = [Line | MoreRules],
            partition_lines(More, rules, MoreRules, Lists)
        )
    ).

partition_lines([Line | More], lists, Rules, [Line | MoreLists]) :-
    partition_lines(More, lists, Rules, MoreLists).


parse_rule(String, LHS-RHS) :-
    re_matchsub("(\\d+)\\|(\\d+)", String, Match),
    number_string(LHS, Match.1),
    number_string(RHS, Match.2).

intern_rule_string(String) :-
    parse_rule(String, LHS-RHS),
    % format('before(~w, ~w)~n', [LHS, RHS]),
    assertz(before(LHS, RHS)).

parse_list(Line, List) :-
    split_string(Line, ",", "", Substrings),
    maplist([Str, Num]>>number_string(Num, Str), Substrings, List).

load_file(Path, Lists) :-
    read_file_lines(Path, Lines),
    partition_lines(Lines, rules, RuleStrs, ListStrs),
    abolish(before/2),
    maplist(intern_rule_string, RuleStrs),
    maplist(parse_list, ListStrs, Lists).

load_example(Lists) :- load_file('day-05-example.txt', Lists).

run_example(Sum) :- load_example(Lists), run(Lists, Sum).

load_input(Lists) :- load_file('day-05.txt', Lists).

run(Sum) :- load_input(Lists), run(Lists, Sum).

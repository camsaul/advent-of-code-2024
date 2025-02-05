:- use_module(library(apply), [foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [member/2, append/3]).
:- use_module(util, [read_file_to_chars/2]).

:- set_prolog_flag(double_quotes, chars).
:- set_prolog_flag(back_quotes, string).

:- discontiguous(solve/4).

% Each stripe can be white (w), blue (u), black (b), red (r), or green (g)

stripe(w).
stripe(u). % blue
stripe(b). % black
stripe(r).
stripe(g).

stripe(Char) --> [Char], { stripe(Char) }.

more_pattern([]) --> [].
more_pattern([X|More]) --> stripe(X), more_pattern(More).

pattern([X|More]) --> stripe(X), more_pattern(More).

patterns([]) --> [].
patterns([Pattern]) --> pattern(Pattern).
patterns([Pattern|More]) --> pattern(Pattern), ", ", patterns(More).

towels([]) --> [].
towels([Pattern]) --> pattern(Pattern).
towels([Pattern|More]) --> pattern(Pattern), "\n", towels(More).

file(Patterns, Towels) --> patterns(Patterns), "\n\n", towels(Towels), "\n".

path(example, `day-19-example.txt`).
path(actual, `day-19.txt`).

init(Input, Patterns, Towels) :-
    path(Input, Path),
    read_file_to_chars(Path, Chars),
    phrase(file(Patterns, Towels), Chars),
    !.

:- table(towel_possible/2).

towel_possible(_Patterns, []).

towel_possible(Patterns, Towel) :-
    member(Part1, Patterns),
    append(Part1, Part2, Towel),
    towel_possible(Patterns, Part2).

reduce_towel_possible(Patterns, Towel, N0, N) :-
    towel_possible(Patterns, Towel)
    ->  N #= N0 + 1
    ;   N #= N0.

solve(part1, Patterns, Towels, NumPossible) :- foldl(call(reduce_towel_possible, Patterns), Towels, 0, NumPossible).

solve(Input, What, NumPossible) :- init(Input, Patterns, Towels), solve(What, Patterns, Towels, NumPossible).

:- begin_tests(part_1).

test(example) :-
    solve(example, part1, NumPossible),
    assertion(NumPossible == 6).

test(actual) :-
    solve(actual, part1, NumPossible),
    assertion(NumPossible == 300).

:- end_tests(part_1).

%
% Part 2
%

:- table(num_possible_patterns/3).

num_possible_patterns(_Patterns, [], 1).

num_possible_patterns(Patterns, Towel, NumPatterns) :-
    findall(N,
            (
                member(TowelPattern, Patterns),
                append(TowelPattern, RestTowel, Towel),
                num_possible_patterns(Patterns, RestTowel, N)
            ),
            Ns),
    foldl(plus, Ns, 0, NumPatterns).

reduce_num_possible_patterns(Patterns, Towel, N0, N) :-
    num_possible_patterns(Patterns, Towel, N1),
    N #= N0 + N1.

solve(part2, Patterns, Towels, NumPatterns) :- foldl(call(reduce_num_possible_patterns, Patterns), Towels, 0, NumPatterns).

:- begin_tests(part_2).

test(example) :-
    solve(example, part2, NumPatterns),
    assertion(NumPatterns == 16).

test(actual) :-
    solve(actual, part2, NumPatterns),
    assertion(NumPatterns == 624802218898092).

:- end_tests(part_2).

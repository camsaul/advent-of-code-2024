:- use_module(library(pcre)).
:- use_module(library(clpfd)).

:- set_prolog_flag(re_compile, true).

reduce_match(Match, Acc, NewAcc) :-
    number_string(X, Match.1),
    number_string(Y, Match.2),
    NewAcc #= Acc + (X * Y).

run(String, Result) :- re_foldl(reduce_match, "mul\\((\\d+),(\\d+)\\)", String, 0, Result, []).

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _Length, String),
    close(Stream).

run(Result) :-
    read_file_to_string("day-03.txt", Input),
    run(Input, Result).

run_sample(Result):- run("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))", Result).

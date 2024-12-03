:- use_module(library(pcre)).
:- use_module(library(clpfd)).

:- set_prolog_flag(re_compile, true).

reduce_match(re_match{0:"do()"}, _Enabled-Acc, true-Acc).

reduce_match(re_match{0:"don't()"}, _Enabled-Acc, false-Acc).

reduce_match(re_match{0:_MulMatch, 1: _XString, 2:_YString}, false-Acc, false-Acc).

reduce_match(re_match{0:_MulMatch, 1: XString, 2:YString}, true-Acc, true-NewAcc) :-
    number_string(X, XString),
    number_string(Y, YString),
    NewAcc #= Acc + (X * Y).

run(String, Result) :-
    re_foldl(reduce_match,
             "(?:do(?:n't)?\\(\\))|(?:mul\\((\\d+),(\\d+)\\))",
             String,
             true-0,
             _Enabled-Result,
             []).

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _Length, String),
    close(Stream).

run(Result) :-
    read_file_to_string("day-03.txt", Input),
    run(Input, Result).

run_sample(Result):- run("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))", Result).

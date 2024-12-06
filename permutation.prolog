%! remove_first(List, X, ListWithoutX) is det.
remove_first([X | More], X, More).
remove_first([X | More], Y, [X | NewMore]) :- X #\= Y, remove_first(More, Y, NewMore).

permutation([], []).

permutation([H1 | T1], [H2 | T2]) :-
    length(T1, Length),
    length(T2, Length),
    remove_first(H1, [H2 | T2], NewList2),
    permutation(T1, NewList2).

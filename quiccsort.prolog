:- use_module(library(lists), [append/3]).

% I guess this code below is basically just a version of QuiccSort, idk I just slapped it together.

%! sort_partition(X, Items, Before, After) is det.
%
%  Partition Items into ones that should get sorted Before X and ones that should get sorted After.
sort_partition(_X, [], [], []).

sort_partition(X, [Y | More], Before, [Y | MoreAfter]) :- before(X, Y),    sort_partition(X, More, Before, MoreAfter).
sort_partition(X, [Y | More], [Y | MoreBefore], After) :- \+ before(X, Y), sort_partition(X, More, MoreBefore, After).

%! sorted_list(List, SortedList) is det.
sorted_list([], []).

sorted_list([X | More], SortedList) :-
    sort_partition(X, More, Before, After),
    sorted_list(Before, SortedBefore),
    sorted_list(After, SortedAfter),
    append(SortedBefore, [X], L1),
    append(L1, SortedAfter, SortedList).

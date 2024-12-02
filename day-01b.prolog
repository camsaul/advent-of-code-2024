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

%!  dict_inc_value(OldDict, NewDict, Key) is det.
%
%   NewDict is the OldDict with the value of Key incremented (or set to 1 if it is not present)
dict_inc_value(OldDict, NewDict, Key) :-
    (get_dict(Key, OldDict, OldValue); OldValue = 0),
    NewValue is OldValue + 1,
    NewDict = OldDict.put(Key, NewValue).


%!  counts_dict_from_list(List, Dict) is det.
%
%   Dict is a dicitionary of Item -> NumOccurances for each distinct Item in List.
counts_dict_from_list([], dict{}) :-
    true.

counts_dict_from_list([H | T], Dict) :-
    counts_dict_from_list(T, TailDict),
    dict_inc_value(TailDict, Dict, H).

parse_input(Input, LocationIDs, CountsDict) :-
    split_lines(Input, Lines),
    parse_lines(Lines, LocationIDs, List2),
    counts_dict_from_list(List2, CountsDict).

similarity_score(LocationID, CountsDict, Score) :-
    (get_dict(LocationID, CountsDict, Count); Count = 0),
    Score is LocationID * Count.

similarity_scores([], _, []) :-
    true.

similarity_scores([LocationIDsH | LocationIDsT], CountsDict, [ScoresH | ScoresT]) :-
    similarity_score(LocationIDsH, CountsDict, ScoresH),
    similarity_scores(LocationIDsT, CountsDict, ScoresT).

reduce_sum([], 0).

reduce_sum([X, Y | More], N) :-
    SumXY is X + Y,
    reduce_sum(More, SumMore),
    N is SumXY + SumMore.

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

% Try with example input: input(Input), run(Input).
%
% input("
% 3   4
% 4   3
% 2   5
% 1   3
% 3   9
% 3   3").

run(Input) :-
    parse_input(Input, LocationIDs, CountsDict),
    similarity_scores(LocationIDs, CountsDict, Scores),
    reduce_sum(Scores, TotalScore),
    writeln(TotalScore).

run() :-
    read_file_to_string("day-01.txt", Input),
    run(Input).

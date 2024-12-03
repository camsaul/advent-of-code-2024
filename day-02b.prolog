%! split_lines(String, Lines) is det.
split_lines(String, Lines) :-
    split_string(String, "\n", "\n", Lines).

%! split_line(Line, StringParts) is det.
split_line(Line, StringParts) :-
    split_string(Line, "\s+", "\s+", StringParts).

%! parse_parts(StringParts, Levels) is det.
parse_parts([], []).

parse_parts([StringPart | MoreStringParts], [Level | MoreLevels]) :-
    number_string(Level, StringPart),
    parse_parts(MoreStringParts, MoreLevels).

%! parse_line(Line, Levels) is det.
parse_line(Line, Levels) :-
    split_line(Line, StringParts),
    parse_parts(StringParts, Levels).

%! parse_lines(Lines, Reports) is det.
parse_lines([], []) :-
    true.

parse_lines([Line | MoreLines], [Reports | MoreReports]) :-
    parse_line(Line, Reports),
    parse_lines(MoreLines, MoreReports).

%! parse_input(Input, Reports) is det.
parse_input(Input, Reports) :-
    split_lines(Input, Lines),
    parse_lines(Lines, Reports).

%!  levels_are_consecutive(Report) is det.
%
%   Whether the levels in Report are either all increasing or all decreasing.
levels_are_consecutive([], _Increasing) :-
    true.

levels_are_consecutive([_X], _Increasing) :-
    true.

levels_are_consecutive([X, Y | More], true) :-
    Y > X,
    levels_are_consecutive([Y | More], true).

levels_are_consecutive([X, Y | More], false) :-
    X > Y,
    levels_are_consecutive([Y | More], false).

levels_are_consecutive(Levels) :-
    levels_are_consecutive(Levels, _Increasing).

%!  level_changes_are_safe(Report) is det.
%
%   Whether all the levels in Report differ by at least one and at more three.
level_changes_are_safe([]) :-
    true.

level_changes_are_safe([_X]) :-
    true.

level_changes_are_safe([X, Y | More]) :-
    Diff is abs(X - Y),
    Diff >= 1,
    Diff =< 3,
    level_changes_are_safe([Y | More]).

report_is_safe(Report) :-
    levels_are_consecutive(Report),
    level_changes_are_safe(Report).

%! remove_index(List, IndexToRemove, NewList) is det.

remove_index(List, IndexToRemove, NewList) :-
    remove_index(List, 0, IndexToRemove, NewList).

remove_index([], _Index, _IndexToRemove, []) :-
    true.

remove_index([_H | T], IndexToRemove, IndexToRemove, T) :-
    true.

remove_index([H | OldT], Index, IndexToRemove, [H | NewT]) :-
    NextIndex is Index + 1,
    remove_index(OldT, NextIndex, IndexToRemove, NewT).

report_is_safe_with_level_removed(Report, -1) :-
    report_is_safe(Report).

report_is_safe_with_level_removed(Report, IndexToRemove) :-
    remove_index(Report, IndexToRemove, UpdatedReport),
    report_is_safe(UpdatedReport).

report_is_safe_with_any_level_removed(Report, IndexToRemove, Length) :-
    report_is_safe_with_level_removed(Report, IndexToRemove);
    (
        NextIndex is IndexToRemove + 1,
        NextIndex < Length,
        report_is_safe_with_any_level_removed(Report, NextIndex, Length)
    ).

report_is_safe_with_any_level_removed(Report) :-
    length(Report, Length),
    report_is_safe_with_any_level_removed(Report, -1, Length).

%! num_safe_reports(Reports, NumSafeReports) is det.
num_safe_reports([], 0) :-
    true.

num_safe_reports([Report | MoreReports], NumSafe) :-
    num_safe_reports(MoreReports, MoreNumSafe),
    (
        (
            report_is_safe_with_any_level_removed(Report),
            NumSafe is MoreNumSafe + 1
        );
        NumSafe = MoreNumSafe
    ).

run(Input) :-
    parse_input(Input, Reports),
    num_safe_reports(Reports, NumSafe),
    writeln(NumSafe).

read_file_to_string(Path, String) :-
    open(Path, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

run() :-
    read_file_to_string("day-02.txt", Input),
    run(Input).

sample_input(
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9").

run_sample() :-
    sample_input(Input),
    run(Input).

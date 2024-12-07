:- module(util, [read_file_lines_to_strings/2,
                 read_file_lines_to_codes/2,
                 read_file_lines_to_chars/2,
                 replace_nth/4]).

:- use_module(library(readutil), [read_line_to_string/2, read_line_to_codes/2]).
:- use_module(library(lists), [same_length/2, append/3]).

read_stream_lines_to_strings(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_strings(Stream, [Line | MoreLines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_stream_lines_to_strings(Stream, MoreLines).

read_file_lines_to_strings(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_strings(Stream, Lines),
    close(Stream).

read_stream_lines_to_codes(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_codes(Stream, [Line | MoreLines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Line),
    read_stream_lines_to_codes(Stream, MoreLines).

read_file_lines_to_codes(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_codes(Stream, Lines),
    close(Stream).

read_stream_lines_to_chars(Stream, []) :- at_end_of_stream(Stream), !.

read_stream_lines_to_chars(Stream, [Chars | More]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, String),
    string_chars(String, Chars),
    read_stream_lines_to_chars(Stream, More).

read_file_lines_to_chars(Path, Lines) :-
    open(Path, read, Stream),
    read_stream_lines_to_chars(Stream, Lines),
    close(Stream).

replace_nth(Index, List, Element, NewList) :-
    same_length(List, NewList),
    length(Before, Index),
    append(Before, [_ | After], List),
    append(Before, [Element | After], NewList).

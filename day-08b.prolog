:- use_module(library(apply), [include/3, maplist/3, foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, append/3]).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_union/3]).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_chars/4]).

% Basic idea is
%
% 1. Find all the distinct pairs of antennas of the same type
%
% 2. For each pair of antennas calculate the line (as a set of all points within the boundaries of the board ) that
%    includes both antennas
%
% 3. reduce the lines into a single set by unironing them together
%
% I think the trickiest part about this was calculating the line, if you have two points 6-6 and 8-8 then that line
% should include 7-7 so you can't just take the diff between the two points and step by that, 0-0, 2-2, 4-4, 6-6, 8-8 is
% wrong; 0-0, 1-1, 2-2,... is the correct line. So I had to calculate the greatest common denominator to simplify the
% rise/run slope fraction.

% Positions are tuples like RowNum-ColNum.
% Size is a tuple like NumRows-NumCols.
% Antenna is a tuple like Position-AntennaType aka RowNum-ColNum-AntennaType.

position(NumRows-NumCols, Row-Col) :- Row #>= 0, Row #< NumRows, Col #>= 0, Col #< NumCols.

%!  line(BoardSize, Position1, Position2, Line) is det.
%
%   For two positions Position1 and Position2, find the Line that will include these points including all points within
%   the bounds of BoardSize.

gcd(X, 0, X) :- X #\= 0.
gcd(X, Y, GCD) :- Y #\= 0, R #= X mod Y, gcd(Y, R, GCD).

simplify_fraction(Numerator, Denominator, SimplifiedNumerator, SimplifiedDenominator) :-
    gcd(Numerator, Denominator, GCD),
    SimplifiedNumerator #= Numerator // GCD,
    SimplifiedDenominator #= Denominator // GCD.

slope(R1-C1, R2-C2, Rise-Run) :- Rise0 #= R2 - R1, Run0 #= C2 - C1, simplify_fraction(Rise0, Run0, Rise, Run).

point_before(Size, Rise-Run, R1-C1, R2-C2) :- R2 #= R1 - Rise, C2 #= C1 - Run, position(Size, R2-C2).

points_before(Size, Slope, P, Points) :-
    point_before(Size, Slope, P, P2)
->  Points = [P2 | MorePoints],
    points_before(Size, Slope, P2, MorePoints)
;   Points = [].

point_after(Size, Rise-Run, R1-C1, R2-C2) :- R2 #= R1 + Rise, C2 #= C1 + Run, position(Size, R2-C2).

points_after(Size, Slope, P, Points) :-
    point_after(Size, Slope, P, P2)
->  Points = [P2 | MorePoints],
    points_after(Size, Slope, P2, MorePoints)
;   Points = [].

line(Size, P1, P2, Line) :-
    slope(P1, P2, Slope),
    points_before(Size, Slope, P1, Before),
    points_after(Size, Slope, P1, After),
    append([Before, [P1], After], Line).

%!  antenna_pairs(Antennas, Pairs) is det.
%
%   Find all unique combinations of Pairs of Antennas that have the same type.

same_antenna_type(_R1-_C1-Type, _R2-_C2-Type).

antenna_pairs([], []).

antenna_pairs([A1 | More], Pairs) :-
    include({A1}/[A2]>>same_antenna_type(A1, A2), More, Matches),
    maplist({A1}/[A2, [A1, A2]]>>true, Matches, A1Pairs),
    antenna_pairs(More, MorePairs),
    append(A1Pairs, MorePairs, Pairs).

%!  antenna_pair_line_points_set(BoardSize, [Antenna1, Antenna2], PointsSet) is det.
%
%   Given a pair of antennas return the ordered set of points within the bounds of BoardSize on the line that includes
%   both antennas.
antenna_pair_line_points_set(Size, [R1-C1-_T1, R2-C2-_T2], Set) :-
    line(Size, R1-C1, R2-C2, Line),
    list_to_ord_set(Line, Set).

antinode_locations(Antennas, Size, AntinodePoints) :-
    antenna_pairs(Antennas, Pairs),
    maplist(call(antenna_pair_line_points_set(Size)), Pairs, LineSets),
    list_to_ord_set([], PointsSet),
    foldl(ord_union, LineSets, PointsSet, AntinodePoints).

init_antenna(P, Char, P-Char) :- Char \= '.'.

init(Path, Antennas, Size) :- read_file_lines_to_chars(Path, Size, init_antenna, Antennas).

example(Points, N) :-
    init('day-08-example.txt', Antennas, Size),
    antinode_locations(Antennas, Size, Points),
    length(Points, N).

run(N) :-
    init('day-08.txt', Antennas, Size),
    antinode_locations(Antennas, Size, Points),
    length(Points, N).

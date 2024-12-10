:- use_module(library(apply), [maplist/3, foldl/4, include/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2, append/3, reverse/2]).
:- use_module(library(yall)).

:- use_module(util, [indexed/2, read_file_lines_to_chars/2]).

:- set_prolog_flag(double_quotes, chars).

free_block('.').
file_block(Block) :- \+ free_block(Block).

repeat(_Element, 0, []).
repeat(Element, N, [Element | More]) :- N #> 0, NextN #= N - 1, repeat(Element, NextN, More).

expand_input(_FileID, [], []).

file_blocks(FileID, Length, Blocks) :- repeat(FileID, Length, Blocks).
free_blocks(Length, Blocks) :- repeat('.', Length, Blocks).

expand_input(FileID, [FileLength], FileBlocks) :- file_blocks(FileID, FileLength, FileBlocks).

expand_input(FileID,  [FileLength, FreeLength | More], Out) :-
    file_blocks(FileID, FileLength, FileBlocks),
    free_blocks(FreeLength, FreeBlocks),
    NextFileID #= FileID + 1,
    expand_input(NextFileID, More, MoreBlocks),
    append([FileBlocks, FreeBlocks, MoreBlocks], Out).

expand_input(In, Out) :- expand_input(0, In, Out).

% defrag(RFileBlocks, FreeBlocks, UnchangedFileBlocks, UnchangedFreeBlocks, NewFileBlocks, NewFreeBlocks).

defrag([], FreeBlocks, [], FreeBlocks, [], []).
defrag(FileBlocks, [], FileBlocks, [], [], []).

defrag([FileI-FileBlock | MoreFileBlocks], [FreeI-FreeBlock | MoreFreeBlocks], UnchangedFileBlocks, UnchangedFreeBlocks, NewFileBlocks, NewFreeBlocks) :-
    (
        format("Defrag @ ~w ~w~n", [FileI, FreeI]),
        FileI #< FreeI
    )
->  defrag(MoreFileBlocks, MoreFreeBlocks, UnchangedFileBlocks1, UnchangedFreeBlocks1, NewFileBlocks, NewFreeBlocks),
    UnchangedFileBlocks = [ FileI-FileBlock | UnchangedFileBlocks1 ],
    UnchangedFreeBlocks = [ FreeI-FreeBlock | UnchangedFreeBlocks1 ]
;   defrag(MoreFileBlocks, MoreFreeBlocks, UnchangedFileBlocks, UnchangedFreeBlocks, NewFileBlocks1, NewFreeBlocks1),
    NewFileBlocks = [ FreeI-FileBlock | NewFileBlocks1 ],
    NewFreeBlocks = [ FileI-FreeBlock | NewFreeBlocks1 ].

% merge_blocks(Blocks0, Blocks1, Block).

merge_blocks(_Pred, [], [], []).
merge_blocks(_Pred, Blocks, [], Blocks).
merge_blocks(_Pred, [], Blocks, Blocks).

merge_blocks(Pred, [P0-B0 | MoreBlocks0], [P1-B1 | MoreBlocks1], [ PB2 | MoreBlocks2 ]) :-
    call(Pred, P0, P1)
->  PB2 = P0-B0,
    merge_blocks(Pred, MoreBlocks0, [P1-B1 | MoreBlocks1], MoreBlocks2)
;   PB2 = P1-B1,
    merge_blocks(Pred, [P0-B0 | MoreBlocks0], MoreBlocks1, MoreBlocks2).

defrag(RFileBlocks, FreeBlocks, DefraggedBlocks) :-
    defrag(RFileBlocks, FreeBlocks, UnchangedFileBlocks, UnchangedFreeBlocks, NewFileBlocks, NewFreeBlocks),
    merge_blocks(#>, UnchangedFileBlocks, NewFileBlocks, RFileBlocks1),
    merge_blocks(#<, UnchangedFreeBlocks, NewFreeBlocks, FreeBlocks1),
    reverse(RFileBlocks1, FileBlocks1),
    append(FileBlocks1, FreeBlocks1, DefraggedBlocks).

defrag(Disk, Out) :-
    include([_Pos-Block]>>file_block(Block), Disk, FileBlocks),
    include([_Pos-Block]>>free_block(Block), Disk, FreeBlocks),
    reverse(FileBlocks, RFileBlocks),
    defrag(RFileBlocks, FreeBlocks, Out).

reduce_checksum(_I-Block, Acc, Acc) :- free_block(Block).
reduce_checksum(I-Block, Acc0, Acc1) :- file_block(Block), Acc1 #= Acc0 + (Block * I).

checksum(Disk, Checksum) :- foldl(reduce_checksum, Disk, 0, Checksum).

parse_input(Input, Disk) :-
    maplist([Char, N]>>number_string(N, [Char]), Input, Nums),
    expand_input(Nums, Blocks),
    indexed(Blocks, Disk).

solve(Disk, Checksum) :- defrag(Disk, Defragged), checksum(Defragged, Checksum).

example(Disk) :- parse_input("2333133121414131402", Disk).

solve_example(Checksum) :- example(Disk), solve(Disk, Checksum).

input(Disk) :- read_file_lines_to_chars('day-09.txt', [In]), parse_input(In, Disk).

solve(Checksum) :- input(Disk), solve(Disk, Checksum).

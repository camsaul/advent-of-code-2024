:- use_module(library(apply), [maplist/3, foldl/4, include/3, maplist/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [append/3, reverse/2]).
:- use_module(library(yall)).

:- use_module(util, [read_file_lines_to_chars/2]).

:- set_prolog_flag(double_quotes, chars).

expand_input(_FileID, [], []).

init_file(FileID, Length, File) :- File = _{type: file, fileID: FileID, length: Length}.

init_free(Length, Free) :- Free = _{type: free, length: Length}.

expand_input(FileID, [FileLength], [File]) :- init_file(FileID, FileLength, File).

expand_input(FileID,  [FileLength, FreeLength | More], [File, Free | MoreChunks]) :-
    init_file(FileID, FileLength, File),
    init_free(FreeLength, Free),
    NextFileID #= FileID + 1,
    expand_input(NextFileID, More, MoreChunks).

expand_input(In, Out) :- expand_input(0, In, Out).

reduce_add_start_end(ItemIn, I0-ItemsIn, I1-ItemsOut) :-
    put_dict(start, ItemIn, I0, Item1),
    I1 #= I0 + ItemIn.length,
    put_dict(end, Item1, I1, ItemOut),
    append(ItemsIn, [ItemOut], ItemsOut).

add_start_end(In, Out) :- foldl(reduce_add_start_end, In, 0-[], _I-Out).

% merge_chunks(Chunks0, Chunks1, Block).

merge_chunks(_Pred, [], [], []).
merge_chunks(_Pred, Chunks, [], Chunks).
merge_chunks(_Pred, [], Chunks, Chunks).

merge_chunks(Pred, [I0 | More0], [I1 | More1], [ I2 | More2 ]) :-
    call(Pred, I0, I1)
->  I2 = I0,
    merge_chunks(Pred, More0, [I1 | More1], More2)
;   I2 = I1,
    merge_chunks(Pred, [I0 | More0], More1, More2).

dict_before(M1, M2) :- M1.start #< M2.start.
dict_after(M1, M2) :- M1.start #> M2.start.

swap_file(File, [], File, []).

swap_file_(File, Free, NewFile, NewFree, NewLeftover) :-
    % update file
    NewFileEnd #= Free.start + File.length,
    NewFile = _{type: file, start: Free.start, end: NewFileEnd, length: File.length, fileID: File.fileID },
    % update free spaces:
    % the one taking file's place
    NewFree = _{type: free, length: File.length, start: File.start, end: File.end },
    % the leftover unused free space
    LeftoverLength #= Free.length - File.length,
    NewLeftover = _{type: free, length: LeftoverLength, start: NewFileEnd, end: Free.end }.

swap_file(File, [Free | MoreFrees], NewFile, NewFrees) :-
    Free.length #>= File.length,
    % we can swap
    swap_file_(File, Free, NewFile, NewFree, NewLeftover),
    (
        NewLeftover.length #= 0
    ->  merge_chunks(dict_before, [NewFree], MoreFrees, NewFrees)
    ;   merge_chunks(dict_before, [NewLeftover, NewFree], MoreFrees, NewFrees)
    ).

swap_file(File, [Free | MoreFrees], NewFile, NewFrees) :-
    Free.length #< File.length,
    Free.start #< File.start,
    % too small, try more files
    swap_file(File, MoreFrees, NewFile, MoreNewFrees),
    merge_chunks(dict_before, [Free], MoreNewFrees, NewFrees).

% defrag(RFiles, Frees, UnchangedFiles, UnchangedFrees, NewFiles, NewFrees).

defrag([], Frees, [], Frees, [], []).
defrag(Files, [], Files, [], [], []).

defrag([File | MoreFiles], [Free | MoreFrees], [File | UnchangedFiles], [Free | UnchangedFrees], NewFiles, NewFrees) :-
    File.start #< Free.start,
    defrag(MoreFiles, MoreFrees, UnchangedFiles, UnchangedFrees, NewFiles, NewFrees).

defrag([File | MoreFiles], [Free | MoreFrees], UnchangedFiles, UnchangedFrees, NewFiles, NewFrees) :-
    File.start #>= Free.start,
    (
        swap_file(File, [Free | MoreFrees], NewFile, Frees1)
    ->  defrag(MoreFiles, Frees1, UnchangedFiles, UnchangedFrees, NewFiles1, NewFrees),
        merge_chunks(dict_after, [NewFile], NewFiles1, NewFiles)
    ;   defrag(MoreFiles, [Free | MoreFrees], UnchangedFiles1, UnchangedFrees, NewFiles, NewFrees),
        merge_chunks(dict_after, [File], UnchangedFiles1, UnchangedFiles)
    ).

defrag(RFiles, Frees, DefraggedChunks) :-
    defrag(RFiles, Frees, UnchangedFiles, UnchangedFrees, NewFiles, NewFrees),
    merge_chunks(dict_after, UnchangedFiles, NewFiles, RFiles1),
    merge_chunks(dict_before, UnchangedFrees, NewFrees, Frees1),
    reverse(RFiles1, Files1),
    merge_chunks(dict_before, Files1, Frees1, DefraggedChunks).

defrag(Disk, Out) :-
    include([M]>>get_dict(type, M, file), Disk, Files),
    include([M]>>get_dict(type, M, free), Disk, Frees),
    reverse(Files, RFiles),
    defrag(RFiles, Frees, Out).

% file_checksum(Start, Length, Checksum).

file_checksum(_Start, 0, _FileID, 0).

file_checksum(Start, Length, FileID, Checksum) :-
    Length #>= 1,
    NextStart #= Start + 1,
    NextLength #= Length - 1,
    file_checksum(NextStart, NextLength, FileID, NextChecksum),
    Checksum #= (Start * FileID) + NextChecksum.

file_checksum(File, Sum) :- file_checksum(File.start, File.length, File.fileID, Sum).

reduce_checksum(Chunk, Acc, Acc) :- Chunk.type = free.

reduce_checksum(Chunk, Acc0, Acc1) :- Chunk.type = file, file_checksum(Chunk, Sum), Acc1 #= Acc0 + Sum.

checksum(Disk, Checksum) :- foldl(reduce_checksum, Disk, 0, Checksum).

parse_input(Input, Disk) :-
    maplist([Char, N]>>number_string(N, [Char]), Input, Nums),
    expand_input(Nums, Expanded),
    add_start_end(Expanded, Disk).

solve(Disk, Checksum) :- defrag(Disk, Defragged), checksum(Defragged, Checksum).

example(Disk) :- parse_input("2333133121414131402", Disk).

solve_example(Checksum) :- example(Disk), solve(Disk, Checksum).

input(Disk) :- read_file_lines_to_chars('day-09.txt', [In]), parse_input(In, Disk).

solve(Checksum) :- input(Disk), solve(Disk, Checksum).

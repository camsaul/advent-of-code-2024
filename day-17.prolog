:- use_module(library(clpfd)).
:- use_module(library(lists), [nth0/3, append/3]).

:- use_module(util, [read_file_to_chars/2]).

:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- set_prolog_flag(double_quotes, chars).

opcode(0, adv).
opcode(1, bxl).
opcode(2, bst).
opcode(3, jnz).
opcode(4, bxc).
opcode(5, out).
opcode(6, bdv).
opcode(7, cdv).

register_name() --> "A".
register_name() --> "B".
register_name() --> "C".

digit(Char) --> [Char], { char_type(Char, digit) }.

digits([]) --> [].
digits([X|More]) -->  digit(X), digits(More).

number(N) --> digit(X), digits(More), { number_chars(N, [X|More]) }.

register_line(N) --> "Register ", register_name, ": ", number(N).

registers(A, B, C) --> register_line(A), "\n", register_line(B), "\n", register_line(C), "\n".

instruction(OpCode, Arg) --> number(N), { opcode(N,OpCode) }, ",", number(Arg).

instructions([]) --> [].
instructions([Op, Arg]) --> instruction(Op, Arg).
instructions([Op, Arg | More]) --> instruction(Op, Arg), ",", instructions(More).

program(Program) --> "Program: ", instructions(Program).

file(A, B, C, Program) --> registers(A, B, C), "\n", program(Program), "\n".

path(example, 'day-17-example.txt').
path(actual, 'day-17.txt').

% state(A, B, C, PC, Output).

init(Input, Program, state(A, B, C, 0, [])) :-
    path(Input, Path),
    read_file_to_chars(Path, Chars),
    phrase(file(A, B, C, Program), Chars),
    format('Program = ~w~n', [Program]).

%!   argument(OperandType, Arg, State, Value).

argument(literal, Arg, _State, Arg).
argument(combo, Arg, _State, Arg) :- Arg in 0..3.
argument(combo, 4, state(A, _, _, _, _), A).
argument(combo, 5, state(_, B, _, _, _), B).
argument(combo, 6, state(_, _, C, _, _), C).

%!   op(Program, OpCode, Arg, State0, State1);

op(adv, Arg, state(A, B, C, PC, Out), state(NewA, B, C, NextPC, Out)) :-
    argument(combo, Arg, state(A, B, C, PC, Out), V),
    NewA #= A // (2 ^ V),
    NextPC #= PC + 2.

op(bxl, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    argument(literal, Arg, state(A, B, C, PC, Out), V),
    NewB #= B xor V,
    NextPC #= PC + 2.

op(bst, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    argument(combo, Arg, state(A, B, C, PC, Out), V),
    NewB #= V mod 8,
    NextPC #= PC + 2.

op(jnz, Arg, state(A, B, C, PC, Out), state(A, B, C, NextPC, Out)) :-
    A #= 0
->  NextPC #= PC + 2
;   argument(literal, Arg, state(A, B, C, PC, Out), NextPC).

op(bxc, _Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    NewB #= B xor C,
    NextPC #= PC + 2.

op(out, Arg, state(A, B, C, PC, Out), state(A, B, C, NextPC, NextOut)) :-
    argument(combo, Arg, state(A, B, C, PC, Out), V0),
    V #= V0 mod 8,
    append(Out, [V], NextOut),
    NextPC #= PC + 2.

op(bdv, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    argument(combo, Arg, state(A, B, C, PC, Out), V),
    NewB #= A // (2 ^ V),
    NextPC #= PC + 2.

op(cdv, Arg, state(A, B, C, PC, Out), state(A, B, NewC, NextPC, Out)) :-
    argument(combo, Arg, state(A, B, C, PC, Out), V),
    NewC #= A // (2 ^ V),
    NextPC #= PC + 2.

at_end_of_program(Program, state(_, _, _, PC, _)) :- length(Program, PC).

step(Program, State, State) :- at_end_of_program(Program, State).

step(Program, State0, State) :-
    \+ at_end_of_program(Program, State0),
    state(A, B, C, PC, Out) = State0,
    nth0(PC, Program, OpCode),
    ArgIndex #= PC + 1,
    nth0(ArgIndex, Program, Arg),
    % format('OpCode = ~w~n', [OpCode]),
    % format('Arg = ~w~n', [Arg]),
    op(OpCode, Arg, state(A, B, C, PC, Out), State1),
    % format('State1 = ~w~n', [State1]),
    step(Program, State1, State).

run(Input) :-
    init(Input, Program, State0),
    step(Program, State0, state(_, _, _, _, Out)),
    format('Out0 = ~w~n', [Out]).

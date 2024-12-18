:- use_module(library(apply), [foldl/4, include/3]).
:- use_module(library(clpfd)).
:- use_module(library(lists), [nth0/3, append/3]).
:- use_module(library(yall)).

:- use_module(util, [read_file_to_chars/2]).

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
path(example_2, 'day-17-example-2.txt').
path(actual, 'day-17.txt').

init(Input, Program, state(A, B, C, 0, [])) :-
    path(Input, Path),
    read_file_to_chars(Path, Chars),
    phrase(file(A, B, C, Program), Chars).

%!   combo(Arg, State, Value)

combo(1, _State, 1).
combo(2, _State, 2).
combo(3, _State, 3).
combo(4, state(A, _, _, _, _), A).
combo(5, state(_, B, _, _, _), B).
combo(6, state(_, _, C, _, _), C).

%!   op(Program, OpCode, Arg, State0, State1)

op(adv, Arg, state(A, B, C, PC, Out), state(NewA, B, C, NextPC, Out)) :-
    combo(Arg, state(A, B, C, PC, Out), V),
    NewA #= A // (2 ^ V),
    NextPC #= PC + 2.

op(bxl, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    NewB #= B xor Arg,
    NextPC #= PC + 2.

op(bst, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    combo(Arg, state(A, B, C, PC, Out), V),
    NewB #= V mod 8,
    NextPC #= PC + 2.

op(jnz, _Arg, state(0, B, C, PC, Out), state(0, B, C, NextPC, Out)) :- NextPC #= PC + 2.
op(jnz, Arg, state(A, B, C, _PC, Out), state(A, B, C, Arg, Out)) :- A #> 0.

op(bxc, _Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    NewB #= B xor C,
    NextPC #= PC + 2.

op(out, Arg, state(A, B, C, PC, Out), state(A, B, C, NextPC, NextOut)) :-
    combo(Arg, state(A, B, C, PC, Out), V0),
    V #= V0 mod 8,
    append(Out, [V], NextOut),
    NextPC #= PC + 2.

op(bdv, Arg, state(A, B, C, PC, Out), state(A, NewB, C, NextPC, Out)) :-
    combo(Arg, state(A, B, C, PC, Out), V),
    NewB #= A // (2 ^ V),
    NextPC #= PC + 2.

op(cdv, Arg, state(A, B, C, PC, Out), state(A, B, NewC, NextPC, Out)) :-
    combo(Arg, state(A, B, C, PC, Out), V),
    NewC #= A // (2 ^ V),
    NextPC #= PC + 2.

at_end_of_program(Program, state(_, _, _, PC, _)) :- length(Program, PC).

step(Program, State, [], State) :- at_end_of_program(Program, State).

nth_instruction(Program, N, OpCode, Arg) :- nth0(N, Program, OpCode), N1 #= N + 1, nth0(N1, Program, Arg).

step(Program, State0, [State1 | MoreStates], EndState) :-
    \+ at_end_of_program(Program, State0),
    state(A, B, C, PC, Out) = State0,
    state(_, _, _, _, EndOut) = EndState,
    append(Out, _, EndOut), % output in this state must be a prefix of the output in the final state
    nth_instruction(Program, PC, OpCode, Arg),
    op(OpCode, Arg, state(A, B, C, PC, Out), State1),
    step(Program, State1, MoreStates, EndState).

run(Input, Out) :-
    init(Input, Program, State0),
    step(Program, State0, _IntermediateStates, state(_, _, _, _, Out)),
    format('Out = ~w~n', [Out]).

raw_program([], []).
raw_program([OpCode, Arg | More], [RawOpCode, Arg | MoreRaw]) :- opcode(RawOpCode, OpCode), raw_program(More, MoreRaw).

labelable_vars(Vars0, Vars) :- include([Var]>>(var(Var), fd_dom(Var, Min..Max), Min \= sup, Max \= sup), Vars0, Vars).

run_2(Input, A) :-
    init(Input, Program, state(_, B, C, PC, Out)),
    raw_program(Program, EndOut),
    EndState = state(_, _, _, _, EndOut),
    step(Program, state(A, B, C, PC, Out), IntermediateStates, EndState),
    !,
    foldl([state(A, B, C, _, _), Acc0, Acc1]>>append(Acc0, [A, B, C], Acc1),
          [state(A, B, C, PC, Out), EndState | IntermediateStates ],
          [],
          Vars0),
    labelable_vars(Vars0, Vars),
    labeling([ffc, min(A)], Vars).

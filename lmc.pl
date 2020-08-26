%Marco Gatti 
%Marco Caspani



:- dynamic
    label/2.

%LMC_RUN
lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Istruction),
    make_memory(Istruction, Memory),
    execution_loop(state(0, 0, Memory, Input, [], noflag), Output),
    !.

%LMC_LOAD
lmc_load(Filename, Mem) :-
     open_file(Filename, List1),
     delete(List1, [], List2),
     assembly(List2, Mem, 0),
     retractall(label(_, _)),
     !.

%ONE ISTRUCTION
one_instruction(State, NewState) :-
    State = state(Acc, Pc, Mem, In, Out, Flag),
    is_acc(Acc),
    queue(In),
    fetch(Acc, Pc, Mem, In, Out, Flag, NewState),
    NewState =.. L,
    nth0(5, L, NewOut, _),
    queue(NewOut),
    !.

%EXECUTION LOOP
execution_loop(State, Out) :-
    one_instruction(State, NewState),
    execution_loop(NewState, Out),
    !.

execution_loop(State, Out) :-
    State = halted_state(_, _, _, _, Out, _),
    !.


%ISTRUZIONI
%loop fetch-execute
%FETCH
fetch(Acc, Pc, Mem, In, Out, Flag, NewState) :-
    State = [Acc, Pc, Mem, In, Out, Flag],
    next_program_counter(Pc, Next_Pc),
    nth0(Pc, Mem, Value, _),
    is_value(Value),
    number_codes(Value, Cod_Value),
    !,
    decode(Cod_Value, State, Next_Pc, NewState).

%DECODE
decode([Cod_istr | Value], State, Next_pc, NewState) :-
    execute(Cod_istr, Value, State, Next_pc, NewState).

decode(Cod_istr, State, Next_pc, NewState) :-
    number_codes(Number_istr, Cod_istr),
    between(0, 99, Number_istr),
    execute(48, _, State, Next_pc, NewState).

%EXECUTE
%ADDIZIONE
execute(49, COD_Numero_Cella, State, Next_Pc, NewState) :-
    State = [Acc, _, Mem, In, Out, _],
    COD_Numero_Cella = [_, _],
    number_codes(Numero_Cella, COD_Numero_Cella),
    nth0(Numero_Cella, Mem, Valore_Cella, _),
    New_Acc is Acc + Valore_Cella,
    flag(New_Acc, New_Flag),
    Acc_Mod is New_Acc mod 1000,
    NewState = state(Acc_Mod, Next_Pc, Mem, In, Out, New_Flag),
    !.

%SOTTRAZIONE
execute(50, COD_Numero_Cella, State, Next_Pc, NewState) :-
    State = [Acc, _, Mem, In, Out, _],
    COD_Numero_Cella = [_, _],
    number_codes(Numero_Cella, COD_Numero_Cella),
    nth0(Numero_Cella, Mem, Valore_Cella, _),
    New_Acc is Acc - Valore_Cella,
    flag(New_Acc, New_Flag),
    Acc_Mod is New_Acc mod 1000,
    NewState = state(Acc_Mod, Next_Pc, Mem, In, Out, New_Flag),
    !.

%STORE
execute(51, COD_Numero_Cella, State, Next_Pc, NewState) :-
     State = [Acc, _, Mem, In, Out, Flag],
     COD_Numero_Cella = [_, _],
     number_codes(Numero_Cella, COD_Numero_Cella),
     replace(Acc, Numero_Cella, Mem, NewMem),
     NewState = state(Acc, Next_Pc, NewMem, In, Out, Flag),
     !.

%LOAD
execute(53, COD_Numero_Cella, State, Next_Pc, NewState) :-
     State = [_, _, Mem, In, Out, Flag],
     COD_Numero_Cella = [_, _],
     number_codes(Numero_Cella, COD_Numero_Cella),
     nth0(Numero_Cella, Mem, NewAcc, _),
     NewState = state(NewAcc, Next_Pc, Mem, In, Out, Flag),
     !.

%BRANCH
execute(54, COD_Value, State, _, NewState) :-
    State = [Acc, _, Mem, In, Out, Flag],
    COD_Value = [_, _],
    number_codes(Value, COD_Value),
    NewState = state(Acc, Value, Mem, In, Out, Flag),
    !.

%BRANCH IF ZERO
execute(55, COD_Value, State, _, NewState) :-
    State = [0, _, Mem, In, Out, noflag],
    COD_Value = [_, _],
    number_codes(Value, COD_Value),
    NewState = state(0, Value, Mem, In, Out, noflag),
    !.

execute(55, _,  State, Next_Pc, NewState) :-
    State = [Acc, _, Mem, In, Out, Flag],
    NewState = state(Acc, Next_Pc, Mem, In, Out, Flag).

%BRANCH IF POSITIVE
execute(56, COD_Value, State, _, NewState) :-
    State = [Acc, _, Mem, In, Out, noflag],
    COD_Value = [_, _],
    number_codes(Value, COD_Value),
    NewState = state(Acc, Value, Mem, In, Out, noflag),
    !.

execute(56, _,  State, Next_Pc, NewState) :-
    State = [Acc, _, Mem, In, Out, Flag],
    NewState = state(Acc, Next_Pc, Mem, In, Out, Flag).

%INPUT
execute(57, COD_Value, State, Next_Pc, NewState) :-
    State = [_, _, Mem, In, Out, Flag],
    number_codes(1, COD_Value),
    In = [X | Y],
    NewState = state(X, Next_Pc, Mem, Y, Out, Flag),
    !.

%OUTPUT
execute(57, COD_Value, State, Next_Pc, NewState) :-
    State = [Acc, _, Mem, In, Out, Flag],
    number_codes(2, COD_Value),
    append(Out, [Acc], NewOut),
    NewState = state(Acc, Next_Pc, Mem, In, NewOut, Flag),
    !.

%HALT
%Halt not produce PC increment, as request
execute(48, _, State, _, NewState) :-
    State = [Acc, Pc, Mem, In, Out, Flag],
    NewState = halted_state(Acc, Pc, Mem, In, Out, Flag).

%non-existent instruction
execute(_, _, _, _, _) :-
    false.

%flag
flag(Value, noflag) :-
    between(0, 999, Value),
    !.

flag(_, flag).

%value in memory
is_value(Value) :-
    integer(Value),
    between(0, 999, Value),
    !.

is_value(_) :-
    false.

%PC
next_program_counter(99, 0) :-
    !.

next_program_counter(X, Y) :-
    Y is X + 1.

%set label, use of dynamic memory
set_label([X, Y, Z], Numero_Cella, [Y, Z]) :-
    not(atom_number(X, _)),
    list_istructions(List),
    not(member(X, List)),
    asserta(label(X, Numero_Cella)),
    !.

set_label([X, Y], Numero_Cella,  [Y]) :-
    not(atom_number(X, _)),
    list_istructions(List),
    not(member(X, List)),
    asserta(label(X, Numero_Cella)),
    !.

set_label([X, Y], _,  [X, Y]) :-
    !.

set_label([X], _, [X]).

%is_label
is_label([Value], Val) :-
    label(Value, Val),
    !.
is_label([Value], Value_Num) :-
    atom_number(Value, Value_Num).

%list of instructions
list_istructions(List) :-
    List1 = ["ADD", "SUB", "STA", "LDA", "BRA", "BRZ"],
    List2 = ["BRP", "INP", "OUT", "HLT", "DAT"],
    append(List1, List2, List),
    !.

%translation in assembly language
%end of file
set_instruction(["END_OF_FILE"], 0) :-
    !.

%ADD
set_instruction(["ADD" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(1, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["ADD" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(1, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%SUB
set_instruction(["SUB" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(2, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["SUB" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(2, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%STA
set_instruction(["STA" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(3, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["STA" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(3, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%LDA
set_instruction(["LDA" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(5, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["LDA" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(5, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%BRA
set_instruction(["BRA" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(6, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["BRA" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(6, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%BRZ
set_instruction(["BRZ" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(7, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["BRZ" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(7, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%BRP
set_instruction(["BRP" | Value], Cod) :-
    is_label(Value, Val),
    Val < 10,
    atomic_concat(0, Val, Val1),
    atomic_concat(8, Val1, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

set_instruction(["BRP" | Value], Cod) :-
    is_label(Value, Val),
    atomic_concat(8, Val, Value_COD),
    atom_number(Value_COD, Cod),
    integer(Cod),
    !.

%DAT
set_instruction(["DAT" | Value], ValN) :-
    Value = [Val],
    atom_number(Val, ValN),
    is_value(ValN),
    !.

set_instruction(["DAT"], 0) :-
    !.

%IN
set_instruction(["INP"], 901) :-
    !.

%OUT
set_instruction(["OUT"], 902) :-
    !.

%HLT
set_instruction(["HLT"], 0) :-
    !.

%unrecognized instruction
set_instruction(_, _) :-
    false.

%replaces nth element of the list
replace(Value, 0, [_ | Rest], [Value | Rest]) :-
    !.

replace(Value, N, [A | B], [A | NewList]) :-
    N > 0,
    N1 is N -1,
    replace(Value, N1, B, NewList).

%make memory
make_memory(Istruction, Memory) :-
    make_memory(Istruction, 0, [], Memory),
    !.

make_memory([], 99, Mem, Memory) :-
    !,
    append(Mem, [0], Memory).

make_memory(Istruction, 99, Mem, Memory) :-
    !,
    Istruction = [Value],
    is_value(Value),
    append(Mem, Istruction, Memory).

make_memory([], Counter, Mem, Memory) :-
    append(Mem, [0], Mem1),
    Counter1 is Counter + 1,
    make_memory([], Counter1, Mem1, Memory).

make_memory([X | Y], Counter, Mem, Memory) :-
    is_value(X),
    append(Mem, [X], Mem1),
    Counter1 is Counter + 1,
    make_memory(Y, Counter1, Mem1, Memory).

%read file
open_file(Filename, List) :-
    open(Filename, read, In),
    !,
    read_string(In, _, String),
    upcase_atom(String, String1),
    split_string(String1, "\n", "", String2),
    splitt(String2, List1),
    delete(List1, [], List),
    close(In),
    !.

open_file(_, _) :-
    write("File error"),
    false.

%assembly
assembly([], [], _) :-
    !.

assembly([X], Mem, 99) :-
    !,
    set_label(X, 99, Rest_value),
    set_instruction(Rest_value, Machine_code),
    append([Machine_code], [], Mem).

assembly([X | Y], Mem, Counter) :-
    Counter < 99,
    !,
    set_label(X, Counter, Rest_value),
    Counter1 is Counter + 1,
    assembly(Y, Mem_Y, Counter1),
    set_instruction(Rest_value, Machine_code),
    append([Machine_code], Mem_Y, Mem).

assembly(_, _, _) :-
    write("Too many instruction"),
    false.

%Split string
whitespace(X) :-
    findall(X, char_type(X, space), Spaces),
    member(X, Spaces),
    !.

first_word([], [], []) :-
    !.

first_word([C | Cs], [], Cs) :-
    whitespace(C),
    !.

first_word([C | Cs], [C | Xs], Rest) :-
    first_word(Cs, Xs, Rest).

split_string_into_words([], []) :-
    !.

split_string_into_words([C | Cs], Ws) :-
    whitespace(C),
    !,
    split_string_into_words(Cs, Ws).

split_string_into_words(CharCodes, [W | Ws]) :-
    first_word(CharCodes, WCodes, Rest),
    string_codes(W, WCodes),
    split_string_into_words(Rest, Ws).

split(String, Res) :-
    split_string(String, "//", "", [String2 | _]),
    string_codes(String2, CharCodes),
    split_string_into_words(CharCodes, Res),
    !.

splitt([], []) :-
    !.

splitt([A | B], [C | D]) :-
    split(A, C),
    splitt(B, D).

%control of values queue
queue([]) :-
    !.

queue([X | C]) :-
    is_value(X),
    queue(C).

%is_acc
is_acc(Acc) :-
    between(0, 999, Acc).

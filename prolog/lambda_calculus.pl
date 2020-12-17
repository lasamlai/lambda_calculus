:- module(lambda_calculus,[
              lambda_compile/0,
              lambda_main/1,
              read_lambda/1,
              proc/0,
              proc_string/1,
              proc_string_clear/1
          ]).

:- use_module(pritty_print).
:- use_module(reduction).
:- use_module(semantic).

:- volatile lambda_compile/0.

lambda_compile:-
    qsave_program(lambda_calculus,[
                      goal(true),
                      toplevel(main),
                      stand_alone(true),
                      undefined(error),
                      foreign(save),
                      verbose(true)
                  ]).

lambda_main(A):-
    main(A).

main([_|P]):-
    arg_parse(P,[]),
    proc.

arg_parse --> ['-d'],!,{debug(lambda_calculus)}, arg_parse.
arg_parse --> [].

proc:-
    read_lambda(A),!,
    proc(A).

proc(A):-
    (   reduction(f([],A),f([],B))
    ->  debug(lambda_calculus,"~@\n",write_lambda(B)),
        proc(B)
    ;   nl,
        write_lambda(A),nl,
        proc
    ).

proc_string(S):-
    string_lambda(S,L),
    proc_reduction_(L,R),
    write_lambda(R),nl.

proc_string_clear(S):-
    string_lambda(S,L),
    proc_reduction_clear_(L,R),
    write_lambda(R),nl.

proc_reduction_clear_(A,C):-
    reduction(f([],A),f([],B)),!,
    shell(clear),
    write_lambda(B),nl,
    proc_reduction_clear_(B,C).
proc_reduction_clear_(A,A).

%!  read_lambda(-Lambda)
% should be det

read_lambda(L):-
    read_string(user_input, "\n", "\r", 10, S),
    string_to_atom(S,A),
    ignore(rl_add_history(A)),
    string_lambda(S,L).

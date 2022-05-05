:- module(reduction,[
              reduction/3,
              fix_reduction/2
          ]).

:- use_module(semantic, [lambda_eq/2]).
:- use_module(pritty_print).

fix_reduction(A, C):-
    reduction([], A, B),
    !,
    debug(lambda_calculus,"~@\n",write_lambda(B)),
    fix_reduction(B, C).

fix_reduction(A, A).

%!  reduction(In, Out)

reduction(_, A, _):-
    var(A),
    !,
    fail.
reduction(_, _, V):-
    nonvar(V),
    !,
    throw(error(V, "Output should be variable!")).
reduction(_, l(A,_), _):-
    nonvar(A),
    !,
    throw(error(A, "lvalue should be a variable!")).

reduction(U, E1, E2) :-
    get_flag(lambda_calculus:strategy, F),
    strategy(F, CC),
    call_or(CC, U, E1, E2).

call_or([], _, _, _) :-
    fail.
call_or([C|_], U, E1, E2) :-
    call(C, U, E1, E2),
    !.
call_or([_|T], U, E1, E2) :-
    call_or(T, U, E1, E2).

strategy(cbv, [app_left_reduction,
               app_right_reduction,
               beta_reduction
              ]).
strategy(cbn, [app_left_reduction,
               beta_reduction,
               app_right_reduction
              ]).
strategy(no, [app_left_reduction,
              reduction_in_lambda,
              beta_reduction,
              app_right_reduction
             ]).
strategy(0, [reduction_in_lambda,
             beta_reduction,
             reduction_both
            ]).

/* reduction in application (left) */
app_left_reduction(U, a(A,B), a(C,B)):-
    reduction(U, A, C).

/* reduction in application (right) */
app_right_reduction(W, a(V,A), a(V,B)):-
    reduction(W, A, B).

/* reduction in lambda */
reduction_in_lambda(U, l(A,B), l(A,D)):-
    reduction([A|U], B, D).

/* beta-reduction */
beta_reduction(U, a(L,A), CC):-
    nonvar(L),
    l(W,C) = L,
    !,
    copy_term(vars_context(U,l(W,C)), vars_context(U,l(A,CC))).

/* special */
beta_reduction(_, a(L,L2), LL):-
    nonvar(L),
    a(EQ,L1) = L,
    EQ == 'EQ',
    !,
    (   \+ \+ lambda_eq(L1, L2)
    ->  LL = l(C, l(_, C))
    ;   LL = l(_, l(C, C))).
/*
beta_reduction(f(U,a(write,K)),f(U,l(C, l(_, C)))):-
    !,
    write_lambda(K).
beta_reduction(f(U,a(put_char,K)),f(U,l(C, l(_, C)))):-
    !,
    lambda_n(K,N),
    put_char(N).
*/

/* reduction in application (both) */
reduction_both(U, a(A,B), a(C,D)):-
    reduction(U, A, C),
    (   reduction(U, B, D)
    ->  true
    ;   B=D).
reduction_both(U, a(A,B), a(A,D)):-
    reduction(U, B, D).

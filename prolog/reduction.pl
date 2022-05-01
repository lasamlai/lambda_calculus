:- module(reduction,[
              reduction/2,
              fix_reduction/2
          ]).

:- use_module(semantic, [lambda_eq/2]).
:- use_module(pritty_print).

fix_reduction(A, C):-
    reduction(f([],A),f([],B)),
    !,
    debug(lambda_calculus,"~@\n",write_lambda(B)),
    fix_reduction(B, C).

fix_reduction(A, A).

%!  reduction(In, Out)

reduction(f(_,A),_):-
    var(A),
    !,
    fail.
reduction(_, f(_, V)):-
    nonvar(V),
    !,
    throw(error(V, "Output should be variable!")).
reduction(f(_,l(A,_)),_):-
    nonvar(A),
    !,
    throw(error(A, "lvalue should be a variable!")).

reduction(E1, E2) :-
    get_flag(lambda_calculus:strategy, F),
    strategy(F, CC),
    call_or(CC, E1, E2).

call_or([], _, _) :-
    fail.
call_or([C|_], E1, E2) :-
    call(C, E1, E2),
    !.
call_or([_|T], E1, E2) :-
    call_or(T, E1, E2).

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
app_left_reduction(f(U,a(A,B)),f(K,a(C,B))):-
    reduction(f(U,A),f(K,C)).

/* reduction in application (right) */
app_right_reduction(f(W,a(V,A)),f(R,a(V,B))):-
    reduction(f(W,A),f(R,B)).

/* reduction in lambda */
reduction_in_lambda(f(U,l(A,B)),f(K,l(AA,D))):-
    reduction(f([A|U],B),f([AA|K],D)).

/* beta-reduction */
beta_reduction(f(U,a(L,A)),f(U,CC)):-
    nonvar(L),
    l(W,C) = L,
    !,
    copy_term(f(U,l(W,C)), f(U,l(A,CC))).

/* special */
beta_reduction(f(U,a(L,L2)), f(U, LL)):-
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
reduction_both(f(U,a(A,B)),f(G,a(C,D))):-
    reduction(f(U,A),f(K,C)),
    (   reduction(f(K,B),f(G,D))
    ->  true
    ;   K=G,B=D).
reduction_both(f(U,a(A,B)),f(K,a(A,D))):-
    reduction(f(U,B),f(K,D)).

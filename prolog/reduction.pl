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

reduction(f(_,A),_):-
    var(A),
    !,
    fail.
reduction(f(W,a(V,A)),f(R,a(V,B))):-
    var(V),
    !,
    reduction(f(W,A),f(R,B)).

/* special */
/*
reduction(f(U,a(write,K)),f(U,l(C, l(_, C)))):-
    !,
    write_lambda(K).
*/
/*
reduction(f(U,a(put_char,K)),f(U,l(C, l(_, C)))):-
    !,
    lambda_n(K,N),
    put_char(N).
*/

reduction(f(U,l(A,B)),f(K,l(AA,D))):-
    !,
    reduction(f([A|U],B),f([AA|K],D)).

reduction(A,C):-
    reduction_l(A,C),
    !.
/*
reduction(A,C):-
    (   reduction_a(A,B)
    ->  reduction_l(B,C)
    ;   reduction_l(A,C)).*/
reduction(A,C):-
    reduction_a(A,C).

reduction(f(U,a(a(EQ,L1),L2)), f(U, LL)):-
    EQ == 'EQ',
    !,
    (   \+ \+ lambda_eq(L1, L2)
    ->  LL = l(C, l(_, C))
    ;   LL = l(_, l(C, C))).

/* beta-reduction */
reduction_l(f(U,a(l(W,C),A)),f(U,C)):-
    copy_term(f(U,A), f(U, W)).

reduction_a(f(U,a(A,B)),f(G,a(C,D))):-
    reduction(f(U,A),f(K,C)),
    (   reduction(f(K,B),f(G,D))
    ->  true
    ;   K=G,B=D).
reduction_a(f(U,a(A,B)),f(K,a(A,D))):-
    reduction(f(U,B),f(K,D)).

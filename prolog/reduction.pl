:- module(reduction,[
              reduction/2,
              proc_reduction_/2
          ]).

:- use_module(semantic, [lambda_eq/4]).

proc_reduction_(A,C):-
    reduction(f([],A),f([],B)),!,
    proc_reduction_(B,C).
proc_reduction_(A,A).

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
reduction(f(U,a(write,K)),f(U,l([C|A]-A, l(B-B, C)))):-
    !,
    write_lambda(K).
*/
/*
reduction(f(U,a(put_char,K)),f(U,l([C|A]-A, l(B-B, C)))):-
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

reduction(f(U,a(a('EQ',L1),L2)), f(U, LL)):-
    !,
    (   lambda_eq([], L1, [], L2)
    ->  LL = l([C|A]-A, l(B-B, C))
    ;   LL = l(A-A, l([C|B]-B, C))).

/* main party */
reduction_l(f(U,a(l(W-[],C),A)),f(WW,C)):-
    wyfiltruj(U,A,Z,B),
    maplist({Z,A}/[L,UU]>>copy_term(f(Z,A),f(UU,L)),W,NZ),
    jednocz([B|NZ],WW).

reduction_a(f(U,a(A,B)),f(G,a(C,D))):-
    reduction(f(U,A),f(K,C)),
    (   reduction(f(K,B),f(G,D))
    ->  true
    ;   K=G,B=D).
reduction_a(f(U,a(A,B)),f(K,a(A,D))):-
    reduction(f(U,B),f(K,D)).


jednocz([A],A).
jednocz([H|T],W):-
    jednocz(T,M),
    maplist([A-B,B-C,A-C]>>true,H,M,W).

%!  wyfiltruj(Zmienne,Term,Zawarte,Niezawarte) is det

wyfiltruj([],_,[],[]).
wyfiltruj([P|L],C,[N|NL],[B|BL]):-
    term_variables(C,W),
    wyciongni(P,W,N,B),
    wyfiltruj(L,C,NL,BL).

wyciongni(A-AA,_,N-N,B-B):-
    A == AA,!.
wyciongni([A|P]-PL,W,[A|N]-NL,B-BL):-
    member_eq(A,W),!,
    wyciongni(P-PL,W,N-NL,B-BL).
wyciongni([A|P]-PL,W,N-NL,[A|B]-BL):-
    wyciongni(P-PL,W,N-NL,B-BL).

member_eq(_,V):-
    var(V),!,fail.
member_eq(X,[H|_]):-
    X == H,!.
member_eq(X,[_|L]):-
    member_eq(X,L).

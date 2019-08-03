:- module(lambda_calculus,[
              lambda_compile/0,
              lambda_main/1,
              read_lambda/1,
              write_lambda/1,
              proc/0,
              proc_string/1,
              reduction/2
          ]).

lambda_compile:-
    qsave_program(lambda_calculus,[goal(true),toplevel(main),stand_alone(true)]).

lambda_main(A):-
    main(A).

main(_):-
    proc.

proc:-
    read_lambda(A),!,
    proc(A).

proc(A):-
    (   reduction(f([],A),f([],B))
    ->  %write_lambda(B),nl,
        proc(B)
    ;   nl,
        write_lambda(A),nl,
        proc
    ).

proc_string(S):-
    string_lambda(S,L),
    proc_reduction_(L,R),
    write_lambda(R),nl.

proc_reduction_(A,C):-
    reduction(f([],A),f([],B)),!,
    proc_reduction_(B,C).
proc_reduction_(A,A).

/*
proc:-
    read_lambda(A), !,
    (   reduction(f([],A),f([],B))
    ->  write_lambda(B),nl, !,
        redo_lambda(B), !
    ;   writeln("I can't reduce"),
        write_lambda(A),nl
    ), !,
    proc.

redo_lambda(A):-
    get_single_char(L),
    (   L = 13
    ->  (   reduction(f([],A),f([],B))
        ->  write_lambda(B),nl,!,
            redo_lambda(B)
        ;   writeln("I can't reduce"),
            write_lambda(A),nl,!)
    ;   (L = 101;L=113)
    ->  true
    ;   format("Unknow code ~d\n",[L]),
        redo_lambda(A)
    ).
*/
/*
lambda(A):-
    var(A).
lambda(a(A,B)):-
    lambda(A),
    lambda(B).
lambda(l(A,B)):-
    var(A),
    lambda(B).
lambda(f(LL,B)):-
    maplist(is_list,LL),
    lambda(B).
*/
write_lambda(A):-
    copy_term(A,B),
    make_ground(B,[],_),
    write_lambda_(B).

write_lambda_(L):-
    lambda_const(K,LL),
    lambda_eq([],L,[],LL),!,
    format("(~w)",K).
write_lambda_(L):-
    lambda_n(L,N),!,
    format("(~w)",N).
write_lambda_(L):-
    lambda_pair(L,X,Y),!,
    format("["),
    write_lambda_(X),
    write_lambda_list(Y).
write_lambda_(L):-
    lambda_pair(L,X,Y),!,
    format("<"),
    write_lambda_(X),
    format("|"),
    write_lambda_(Y),
    format(">").
write_lambda_(L):-
    lambda_krot(L,K),!,
    (   K = []
    ->  format("<>")
    ;   K = [A|B],
        format("<"),
        write_lambda_(A),
        write_lambda_krot(B)).

/*
write_lambda_(l([], l([A], A))):-
    format("FALSE").
write_lambda_(l([A], l([], A))):-
    format("TRUE").

write_lambda_(L):-
    lambda_number(L,N),!,
    format("~d",[N]).
*/
write_lambda_(A):-
    var(A),!,
    format("~w",[A]).
write_lambda_(l(L-L,B)):-
    !,
    format("λ_"),
    write_lambda_l(B).
write_lambda_(l([A|_]-[],B)):-
    !,
    format("λ~w",[A]),
    write_lambda_l(B).
write_lambda_(a(A,C)):-
    !,
    write_lambda_n(A),
    write_lambda_k(C).

write_lambda_(A):-
    format("~w",[A]).

% Nawiasuje lambde

write_lambda_n(l(K,W)):-
    !,
    format("("),
    write_lambda_(l(K,W)),
    format(")").
write_lambda_n(A):-
    write_lambda_(A).

% Nawiasuje Aplikacje i lambda
write_lambda_k(l(K,W)):-
    !,
    format("("),
    write_lambda_(l(K,W)),
    format(")").
write_lambda_k(a(K,W)):-
    !,
    format("("),
    write_lambda_(a(K,W)),
    format(")").
write_lambda_k(A):-
    write_lambda_(A).

write_lambda_l(l([],B)):-
    !,
    format("_"),
    write_lambda_l(B).
write_lambda_l(l([A|_],B)):-
    !,
    format("~w",[A]),
    write_lambda_l(B).
write_lambda_l(A):-
    format("."),
    write_lambda_(A).

write_lambda_krot([]):-
    format(">").
write_lambda_krot([A|B]):-
    format("|"),
    write_lambda_(A),
    write_lambda_krot(B).

write_lambda_list(FALSE):-
    lambda_const('FALSE',FALSE),
    !,
    format("]").
write_lambda_list(PAIR):-
    lambda_pair(PAIR,A,B),
    !,
    format(","),
    write_lambda_(A),
    write_lambda_list(B).
write_lambda_list(A):-
    !,
    format("|"),
    write_lambda_(A),
    format("]").

reduction(f(_,A),_):-
    var(A),
    !,
    fail.
reduction(f(W,a(V,A)),f(R,a(V,B))):-
    var(V),
    !,
    reduction(f(W,A),f(R,B)).
/* special */
reduction(f(U,a(write,K)),f(U,l([C|A]-A, l(B-B, C)))):-
    write_lambda(K).
reduction(f(U,a(put_char,K)),f(U,l([C|A]-A, l(B-B, C)))):-
    lambda_n(K,N),
    put_char(N).
/* main party */
reduction(f(U,a(l(W-[],C),A)),f(WW,C)):-
    wyfiltruj(U,A,Z,B),
    maplist({Z,A}/[L,UU]>>copy_term(f(Z,A),f(UU,L)),W,NZ),
    jednocz([B|NZ],WW).
/*
reduction(f(U,a(l(W-[],C),A)),f(WW,C)):-
    maplist({U,A}/[L,UU]>>copy_term(f(U,A),f(UU,L)),W,NU),
    jednocz([U|NU],WW).
*/
reduction(f(U,a(A,B)),f(G,a(C,D))):-
    reduction(f(U,A),f(K,C)),
    (   reduction(f(K,B),f(G,D))
    ->  true
    ;   K=G,B=D).
reduction(f(U,a(A,B)),f(K,a(A,D))):-
    reduction(f(U,B),f(K,D)).
/*
reduction(f(U,a(A,B)),f(K,a(C,B))):-
    reduction(f(U,A),f(K,C)).
reduction(f(U,a(A,B)),f(K,a(A,D))):-
    reduction(f(U,B),f(K,D)).
*/
reduction(f(U,l(A,B)),f(K,l(AA,D))):-
    reduction(f([A|U],B),f([AA|K],D)).

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


%!  read_lambda(-Lambda)
% should be det

read_lambda(L):-
    read_string(user_input, "\n", "\r", 10, S),
    string_lambda(S,L).

string_lambda(S,LL):-
    string_codes(S,CC),
    phrase(lam(L),CC),!,
    lam_var(L,LL,[],[]).

:- use_module(library(dcg/basics)).

% Parsing
%
% ```
% lam :=
%      | L lam_
%      | lam_t
% ```
lam(L) --> ("l";"λ"),!,lam_(L).
lam(A) --> lam_t(A).

%```
%lam_t :=
%       | lam_i
%       | lam_i lam_t2
%```

lam_t(T) --> lam_i(A),(lam_t(A,T);{T=A}).

%```
%lam_t2 :=
%        | lam_i
%        | lam_i lam_t2
%```

lam_t(A,T) --> lam_i(B),(lam_t(a(A,B),T);{T=a(A,B)}).

%```
%lam_i :=
%       | number
%       | "Y"
%       | "(" lam ")"
%       | l_var
%```

lam_i(N) --> number(N).
lam_i('Y') --> "Y".
lam_i('write') --> "write".
lam_i('put_char') --> "put_char".
lam_i(A) --> lam_i_macros(A).
lam_i(pair(A,B)) --> "<",lam(A),",",lam(B),">".
lam_i(krot([A|B])) --> "<",lam(A),lam_i_krot(B).
lam_i(list([])) --> "[]".
lam_i(list([A|B])) --> "[",lam(A),lam_i_list(B).
lam_i(A) --> "(",!,lam(A),")".
lam_i(A) --> l_var(A).

lam_i_krot([]) --> ">",!.
lam_i_krot([A|B]) --> "|",lam(A),lam_i_krot(B).

lam_i_list([]) --> "]",!.
lam_i_list(A) --> "|",!,lam(A),"]".
lam_i_list([A|B]) --> ",",!,lam(A),lam_i_list(B).

%```
%l_var := Char
%```

l_var(A) --> [C],{between(97,122,C)/*;between(65,90,C)*/},!,{atom_codes(A,[C])}.

%```
%lam_ :=
%      | "." lam
%      | "_" lam_
%      | lam_var lam_
%```

lam_(B) -->     ".",lam(B).
lam_(l([],B))-->"_",lam_(B).
lam_(l(A,B))--> l_var(A),lam_(B).


% Tu Analizujemu sparsowane dane


lam_var(N,L,P,P):-
    number(N),
    !,
    lambda_number(L,N),!.
lam_var(pair(V,W),l([B|A]-A, a(a(B, X), Y)),R,P):-
    !,
    lam_var(V,X,R,E),
    lam_var(W,Y,E,P).
lam_var(krot(KK),l([F|H]-H, W),R,P):-
    !,
    reverse(KK,K),
    lam_var_krot(K,F,W,R,P).
lam_var(list(LL),L,R,P):-
    lam_var_list(LL,L,R,P).
lam_var(A,L,P,P):-
    atom(A),
    lambda_const(A,L),!.
/*lam_var('Y',l([G, C|A]-A, a(l([E, D|B]-B, a(C, a(D, E))), l([I, H|F]-F, a(G, a(H, I))))),P,P):-
    !.*/
lam_var(l(A,B),l(M,D),R,P):-
    !,
    lam_var(B,D,[A=L-L|R],[A=M|P]).
lam_var(a(A,B),a(C,D),R,P):-
    !,
    lam_var(A,C,R,E),
    lam_var(B,D,E,P).
lam_var(A,W,R,P):-
    member(A=_,R),
    !,
    add_lists(A,W,R,P).
lam_var(A,A,P,P).

lam_var_krot([],F,F,P,P):-!.
lam_var_krot([A|B],F,a(W,AA),R,P):-
    lam_var(A,AA,R,E),
    lam_var_krot(B,F,W,E,P).

lam_var_list([],L,P,P):-
    lambda_const('FALSE',L),!.
lam_var_list([A|B],L,R,P):-
    !,
    lam_var(A,AA,R,E),
    lambda_pair(L,AA,K),
    lam_var_list(B,K,E,P).
lam_var_list(A,AA,R,P):-
    lam_var(A,AA,R,P).

add_lists(_,_,[],[]):-
    throw("error adding to list").
add_lists(A,Z,[A=L-O|W],[A=[Z|L]-O|W]):-!.
add_lists(A,Z,[H|W],[H|M]):-
    add_lists(A,Z,W,M).

make_ground(A,R,[A|R]):-
    var(A),!,
    nazwa_zmien(A),
    \+ member(A,R),!.
make_ground(l(L-LL,B),R,P):-
    L == LL,
    !,
    make_ground(B,R,P).
make_ground(l(A-[],B),R,P):-
    nazwa_zmien(L),
    \+ member(L,R),!,
    maplist({L}/[N]>>(N=L),A),
    make_ground(B,[L|R],P).
make_ground(a(A,B),R,P):-
    !,
    make_ground(A,R,E),
    make_ground(B,E,P).
make_ground(A,R,R):-
    atom(A).

% Consts
/*
lambda_const(A,B):-
    atom(A),
    var(B),!,
    lambda_const_(A,B).

lambda_const(A,B):-
    var(A),
    lambda_const_(A,B).
*/


lambda_const_add(Atom,String):-
    string_lambda(String,L),
    find_(L,M,[]),
    list_to_and(M,F),
    assertz(:-(lambda_const(Atom,L),F)),
    atom_string(Atom,SS),
    expand_term(lam_i_macros(Atom)-->SS,EE),
    assertz(EE).

find_(A,F,F):-
    var(A),!.
find_(l(_-A,K),[end(A)|F],L):-
    find_(K,F,L).
find_(a(A,B),F,L):-
    find_(A,F,E),
    find_(B,E,L).

list_to_and([A],A):-!.
list_to_and([A|B],(A,C)):-
    list_to_and(B,C).

end(L):-
    var(L),!.
end([]).


lambda_pair(l([B|A]-A, a(a(B,X),Y)),X,Y).

lambda_krot(l([F|A]-A, W),M):-
    lambda_krot_(F,W,[],M).

lambda_krot_(F,FF,A,A):-
    F == FF,!.
lambda_krot_(F,a(W,X),L,M):-
    lambda_krot_(F,W,[X|L],M).

lambda_n(l(F-_,H),N):-
    lambda_n1(F,H,N).
lambda_n1(F,l(X-_,A),N):-
    lambda_n2(F,X,A,N).
lambda_n2(_,X,A,0):-
    (var(A);atom(A)),
    member_eq(A,X).
lambda_n2(F,X,a(FF,A),N):-
    (number(N)->succ(NN,N);true),
    member_eq(FF,F),
    lambda_n2(F,X,A,NN),
    succ(NN,N).

lambda_number(_,N):-
    \+ var(N),
    \+ number(N),!,
    fail.
lambda_number(l(F,l([X|L]-L,M)),N):-
    end(L),
    lambda_number_(F,M,X,N).
lambda_number_(L-LL,X,X,0):-
    end(L),
    L = LL.
lambda_number_([F|K]-L,a(F,M),X,NN):-
    number(NN),!,
    succ(N,NN),
    lambda_number_(K-L,M,X,N).
lambda_number_([F|K]-L,a(F,M),X,NN):-
    lambda_number_(K-L,M,X,N),
    succ(N,NN).

:- dynamic lambda_const/2.
:- dynamic lam_i_macros//1.

:- lambda_const_add('Y',"lf.(lx.f(xx))(lx.f(xx))").
:- lambda_const_add('[]',"l_a.a").
:- lambda_const_add('FALSE',"l_x.x").
:- lambda_const_add('TRUE',"lx_.x").
:- lambda_const_add('Nsucc',"lnfx.f(nfx)").
:- lambda_const_add('NSUCC',"lnfx.nf(fx)").
:- lambda_const_add('Nplus',"lmnfx.mf(nfx)").
:- lambda_const_add('Npred',"lnfx.n(lgh.h(gf))(lu.x)(lu.u)").
:- lambda_const_add('Nminus',"lmn.nNpredm").
:- lambda_const_add('Nmult',"lmnfx.m(nf)x").
:- lambda_const_add('Nexp',"lmn.nm").
:- lambda_const_add('PAIR',"labf.fab").
:- lambda_const_add('AND',"lpq.pqp").
:- lambda_const_add('OR',"lpq.ppq").
:- lambda_const_add('IsNil',"lw.w(lhtd.FALSE)TRUE").
:- lambda_const_add('NIsZero',"ln.n(lx.FALSE)TRUE").
:- lambda_const_add('IsZero',"NIsZero").
:- lambda_const_add('value',"lmn.nm").
:- lambda_const_add('<>',"lx.x").
:- lambda_const_add('Nleq',"lmn.NIsZero(Nminusmn)").
:- lambda_const_add('Neq',"lmn.AND(Nleqmn)(Nleqnm)").
:- lambda_const_add('Svar',"ln.<0|n>").
:- lambda_const_add('Sapp',"lab.<1|<a|b>>").
:- lambda_const_add('Slam',"lvw.<2|<v|w>>").
%:- lambda_const_add('MAP',"ldi.(Neqi(dTRUETRUE))(dTRUE[])(MAP(d[])i)").

:- compile_predicates([lambda_const/2,lam_i_macros//1]).


/**@test (Y(lfx.(IsZerox)12(f(predx))))1
 *
 */

%!  lambda_eq(Vars1,L1,Vars2,L2) is det

lambda_eq(V1,A,V2,B):-
    (var(A);atom(A)),
    (   var(B)
    ->  var_eq(V1,A,V2,B)
    ;   !,fail).
lambda_eq(_,_,_,B):-
    var(B),!,fail.

lambda_eq(V1,a(A,B),V2,a(C,D)):-
    lambda_eq(V1,A,V2,C),
    lambda_eq(V1,B,V2,D).
lambda_eq(V1,l(F,A),V2,l(G,B)):-
    lambda_eq([F|V1],A,[G|V2],B).

var_eq([F-_|_],A,[G-_|_],B):-
    member_eq(A,F),
    (   member_eq(B,G)
    ->  true
    ;   !,fail).
var_eq([_|V1],A,[_|V2],B):-
    var_eq(V1,A,V2,B).

member_eq(_,V):-
    var(V),!,fail.
member_eq(X,[H|_]):-
    X == H,!.
member_eq(X,[_|L]):-
    member_eq(X,L).

% nazwa_zmien(-A) is multi

nazwa_zmien(A):-
    member(A,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,u,p,r,s,t,u,w,y,z]).
nazwa_zmien(A):-
    between(1,inf,N),
    atom_concat('#',N,A).









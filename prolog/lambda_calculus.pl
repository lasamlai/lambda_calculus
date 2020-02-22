:- module(lambda_calculus,[
              lambda_compile/0,
              lambda_main/1,
              read_lambda/1,
              write_lambda/1,
              proc/0,
              proc_string/1,
              proc_string_clear/1,
              reduction/2
          ]).

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
    ->  debug(lambda_calculus,"~@\n\n",write_lambda(B)),
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

%!  write_lambda(Lambda) is det

write_lambda(A):-
    format("\e[39m"),
    \+ \+ write_lambda_(A,a,1).

write_lambda_(L,Z,R):-
    witch_lambda_const_(L,K),
    !,
    write_lambda_unlam(K,Z,R).

write_lambda_(A,_,_):-
    var(A),!,
    format("\e[92m~w\e[39m",[A]).

write_lambda_(l(L1-L2,B),Z,R):-
    L1 == L2,
    !,
    format("\e[91mλ\e[90m_"),
    write_lambda_l(B,Z,R).
write_lambda_(l(L-[],B),Z,R):-
    !,
    maplist({Z}/[Z]>>true,L),
    format("\e[91mλ\e[34m~w",[Z]),
    nazwa_zmien(Z,ZZ),
    write_lambda_l(B,ZZ,R).

write_lambda_(a(A,C),Z,R):-
    !,
    write_lambda_n(A,Z,R),
    write_lambda_k(C,Z,R).

write_lambda_(A,_,_):-
    format("\e[92m~w\e[39m",[A]).

% Nawiasuje lambde

write_lambda_n(l(K,W),Z,R):-
    !,
    format("\e[38;5;~dm(\e[39m",[R]),
    next_color(R,RR),
    write_lambda_(l(K,W),Z,RR),
    format("\e[38;5;~dm)\e[39m",[R]).
write_lambda_n(A,Z,R):-
    write_lambda_(A,Z,R).

% Nawiasuje Aplikacje i lambda
write_lambda_k(l(K,W),Z,R):-
    !,
    format("\e[38;5;~dm(\e[39m",[R]),
    next_color(R,RR),
    write_lambda_(l(K,W),Z,RR),
    format("\e[38;5;~dm)\e[39m",[R]).
write_lambda_k(a(K,W),Z,R):-
    !,
    format("\e[38;5;~dm(\e[39m",[R]),
    next_color(R,RR),
    write_lambda_(a(K,W),Z,RR),
    format("\e[38;5;~dm)\e[39m",[R]).
write_lambda_k(A,Z,R):-
    write_lambda_(A,Z,R).

write_lambda_l(L,Z,R):-
    witch_lambda_const_(L,K),!,
    format("\e[91m.\e[39m"),
    write_lambda_unlam(K,Z,R).
/*
write_lambda_l(L):-
    lambda_n(L,N),!,
    format("\e[91m.\e[39m(\e[36m~w\e[39m)",N).
*/
write_lambda_l(l(L1-L2,B),Z,R):-
    L1 == L2,
    !,
    format("\e[90m_"),
    write_lambda_l(B,Z,R).
write_lambda_l(l(L-[],B),Z,R):-
    !,
    maplist({Z}/[Z]>>true,L),
    nazwa_zmien(Z,ZZ),
    format("\e[34m~w",[Z]),
    write_lambda_l(B,ZZ,R).
write_lambda_l(A,Z,R):-
    format("\e[91m.\e[39m"),
    write_lambda_(A,Z,R).

write_lambda_krot([],_,_):-
    format("\e[93m>\e[39m").
write_lambda_krot([A|B],Z,R):-
    format("\e[93m|\e[39m"),
    write_lambda_(A,Z,R),
    write_lambda_krot(B,Z,R).

write_lambda_list([],_,_):-
    !,
    format("\e[93m]\e[39m").
write_lambda_list([A|B],Z,R):-
    !,
    format("\e[93m,\e[39m"),
    write_lambda_(A,Z,R),
    write_lambda_list(B,Z,R).
write_lambda_list(A,Z,R):-
    !,
    format("\e[93m|\e[39m"),
    write_lambda_(A,Z,R),
    format("\e[93m]\e[39m").

witch_lambda_const_(L,const(K)):-
    lambda_const(K,LL),
    lambda_eq([],L,[],LL).
witch_lambda_const_(L,nat(N)):-
    lambda_n(L,N).
witch_lambda_const_(L,int(Z,K)):-
    is_lambda_pair(L,A,B),
    lambda_n(A,N),
    lambda_n(B,M),!,
    Z is N - M,
    K is min(N,M).
witch_lambda_const_(L,list([H|T])):-
    is_lambda_pair(L,H,Y),!,
    witch_lambda_list(Y,T).
witch_lambda_const_(L,pair(X,Y)):-
    is_lambda_pair(L,X,Y),!.
witch_lambda_const_(L,krot(K)):-
    is_lambda_krot(L,K),!.

witch_lambda_list(FALSE,[]):-
    lambda_const('FALSE',FALSE),!.
witch_lambda_list(PAIR,[H|T]):-
    is_lambda_pair(PAIR,H,Y),
    !,
    witch_lambda_list(Y,T).
witch_lambda_list(A,A).

write_lambda_unlam(const(K),_,_):-
    !,
    format("\e[93m~w\e[39m",[K]).
write_lambda_unlam(nat(N),_,_):-
    !,
    format("(\e[36m~dN\e[39m)",[N]).
write_lambda_unlam(int(N,0),_,_):-
    !,
    format("(\e[36m~dZ\e[39m)",[N]).
write_lambda_unlam(int(N,K),_,_):-
    !,
    format("(\e[36m~dZ~d\e[39m)",[N,K]).
write_lambda_unlam(enum(N,K),_,_):-
    !,
    format("(\e[36m~dE~d\e[39m)",[N,K]).

write_lambda_unlam(list([X|Y]),Z,R):-
    !,
    format("\e[93m[\e[39m"),
    write_lambda_(X,Z,R),
    write_lambda_list(Y,Z,R).
write_lambda_unlam(pair(X,Y),A,R):-
    !,
    format("\e[93m<\e[39m"),
    write_lambda_(X,A,R),
    format("\e[93m|\e[39m"),
    write_lambda_(Y,A,R),
    format("\e[93m>\e[39m").
write_lambda_unlam(krot(K),Z,R):-
    !,
    (   K = []
    ->  format("\e[93m<>\e[39m")
    ;   K = [A|B],
        format("\e[93m<\e[39m"),
        write_lambda_(A,Z,R),
        write_lambda_krot(B,Z,R)).


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
    !,
    write_lambda(K).
reduction(f(U,a(put_char,K)),f(U,l([C|A]-A, l(B-B, C)))):-
    !,
    lambda_n(K,N),
    put_char(N).

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


%!  read_lambda(-Lambda)
% should be det

read_lambda(L):-
    read_string(user_input, "\n", "\r", 10, S),
    string_to_atom(S,A),
    ignore(rl_add_history(A)),
    string_lambda(S,L).

string_lambda(S,LL):-
    string_codes(S,CC),
    codes_lambda(CC,LL).

codes_lambda(CC,LL):-
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
lam(L) --> ("l";"λ";"\\"),!,lam_(L).
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

lam_i(nat(N)) --> number(N),"N".
lam_i(int(N,M)) --> number(N),"Z",number(M).
lam_i(int(N,0)) --> number(N),"Z".
lam_i(nat(N)) --> number(N).
lam_i('Y') --> "Y".
lam_i('write') --> "write".
lam_i('put_char') --> "put_char".
lam_i(A) --> lam_i_macros(A).
lam_i(A) --> up_chars(A).
lam_i(pair(A,B)) --> "<",lam(A),",",lam(B),">".
lam_i(krot([A|B])) --> "<",lam(A),lam_i_krot(B).
lam_i(list([])) --> "[]".
lam_i(list([A|B])) --> "[",lam(A),lam_i_list(B).
lam_i(A) --> "(",!,lam(A),")".
lam_i(A) --> l_var(A).

up_chars(A) -->
    up_chars_(AA),
    {AA \= [],
     atom_codes(A,AA)}.

up_chars_([C|T]) -->
    [C],
    {between(65,90,C)},
    up_chars_(T).
up_chars_([]) --> [].

lam_i_krot([]) --> ">",!.
lam_i_krot([A|B]) --> "|",lam(A),lam_i_krot(B).

lam_i_list([]) --> "]",!.
lam_i_list(A) --> "|",!,lam(A),"]".
lam_i_list([A|B]) --> ",",!,lam(A),lam_i_list(B).

%```
%l_var := Char
%```

l_var(A) --> [C],{between(97,122,C),!,atom_codes(A,[C])}.

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


lam_var(nat(N),L,P,P):-
    !,
    lambda_number(L,N).
lam_var(int(N,M),L,P,P):-
    !,
    (   N >= 0
    ->  N1 is N + M,
        N2 = M
    ;   N1 = M,
        N2 is M - N
    ),
    lambda_number(NL,N1),
    lambda_number(NP,N2),
    get_lambda_pair(L,NL,NP).
lam_var(pair(V,W),l([B|A]-A, a(a(B, X), Y)),R,P):-
    !,
    lam_var(V,X,R,E),
    lam_var(W,Y,E,P).
lam_var(krot(KK),l([F|H]-H, W),R,P):-
    !,
    reverse(KK,K),
    lam_var_krot(K,F,W,R,P).
lam_var(list(LL),L,R,P):-
    !,
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
    get_lambda_pair(L,AA,K),
    lam_var_list(B,K,E,P).
lam_var_list(A,AA,R,P):-
    lam_var(A,AA,R,P).

add_lists(_,_,[],[]):-
    throw("error adding to list").
add_lists(A,Z,[A=L-O|W],[A=[Z|L]-O|W]):-!.
add_lists(A,Z,[H|W],[H|M]):-
    add_lists(A,Z,W,M).
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

:- volatile lambda_add_close_Y/2.

lambda_add_close_Y(Atom,String):-
    string_lambda(String,L),
    proc_reduction_(L,LL),
    lambda_const('Y',Y),!,
    reduction(f([],a(LL,Y)),f([],Lambda)),!,
    lambda_const_add_(Atom,Lambda).

:- volatile lambda_add_fix_close/3.

lambda_add_fix_close(YAtom,Atom,String):-
    string_lambda(String,L),
    proc_reduction_(L,LL),
    lambda_const_add_(Atom,LL),
    lambda_const('Y',Y),!,
    lambda_const_add_(YAtom,a(Y,LL)).

:- volatile lambda_add_close/2.

lambda_add_close(Atom,String):-
    string_lambda(String,L),
    proc_reduction_(L,LL),
    lambda_const_add_(Atom,LL).

:- volatile lambda_const_add/2.

lambda_const_add(Atom,String):-
    string_lambda(String,L),
    find_(L,M,[]),
    list_to_and(M,F),
    assertz(:-(lambda_const(Atom,L),F)),
    atom_string(Atom,SS),
    expand_term(lam_i_macros(Atom)-->SS,EE),
    assertz(EE).

:- volatile lambda_const_add_/2.

lambda_const_add_(Atom,Lambda):-
    find_(Lambda,M,[]),
    list_to_and(M,F),
    assertz(:-(lambda_const(Atom,Lambda),F)),
    atom_string(Atom,SS),
    expand_term(lam_i_macros(Atom)-->SS,EE),
    assertz(EE).

:- volatile find_/3.

find_(A,F,F):-
    var(A),!.
find_(l(_-A,K),[end(A)|F],L):-
    find_(K,F,L).
find_(a(A,B),F,L):-
    find_(A,F,E),
    find_(B,E,L).

:- volatile list_to_and/2.

list_to_and([A],A):-!.
list_to_and([A|B],(A,C)):-
    list_to_and(B,C).

end(L):-
    var(L),!.
end([]).

member_eq(_,V):-
    var(V),!,fail.
member_eq(X,[H|_]):-
    X == H,!.
member_eq(X,[_|L]):-
    member_eq(X,L).

get_lambda_pair(l([B|A]-A, a(a(B,X),Y)),X,Y).

is_lambda_pair(l([B|A1]-A2, a(a(B,X),Y)),X,Y):-
    var(B),
    A1 == A2.

is_lambda_krot(l([F|A1]-A2, W),M):-
    A1 == A2,
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

%!  lambda_number(?Lambda,?Number) is det

lambda_number(_,N):-
    \+ var(N),
    \+ number(N),!,
    fail.
lambda_number(l(F,l([X|L]-L,M)),N):-
    end(L),
    !,
    lambda_number_(F,M,X,N).
lambda_number_(L-LL,X,X,0):-
    end(L),
    !,
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

:- lambda_const_add('<>',"lx.x").
:- lambda_const_add('Y',"lf.(lx.f(xx))(lx.f(xx))").

:- lambda_const_add('Ninf',"lfx.Yf").

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
:- lambda_const_add('NIsZero',"ln.n(lx.FALSE)TRUE").

:- lambda_const_add('PAIR',"labf.fab").
:- lambda_const_add('CAR',"<TRUE>").
:- lambda_const_add('CDR',"<[]>").

:- lambda_const_add('AND',"lpq.pqp").
:- lambda_const_add('OR',"lpq.ppq").
:- lambda_const_add('NOT',"lpab.pba").
:- lambda_const_add('Not',"lp.p[]TRUE").

:- lambda_add_close('XOR',"lpq.p(NOTq)q").
:- lambda_add_close('Xor',"lpq.p(Notq)q").

:- lambda_add_close('NXOR',"lpq.NOT(XORpq)").
:- lambda_add_close('NXor',"lpq.Not(Xorpq)").

:- lambda_const_add('IsNil',"lw.w(lhtd.FALSE)TRUE").

:- lambda_add_close('Nleq',"lmn.NIsZero(Nminusmn)").
:- lambda_add_close('Neq',"lmn.AND(Nleqmn)(Nleqnm)").

:- lambda_const_add('NtoZ',"ln.<n|0>").
:- lambda_add_close('Zneg',"λz.<CDRz|CARz>").
:- lambda_add_fix_close('OneZero','POneZero',"lfz.NIsZero(CDRz)z(NIsZero(CDRz)z(f<Npred(CARz)|Npred(CDRz)>))").

:- lambda_add_close('Zplus',"lxy.<Nplus(CARx)(CARy)|Nplus(CDRx)(CDRy)>").
:- lambda_add_close('Zminus',"lxy.<Nplus(CARx)(CDRy)|Nplus(CDRx)(CARy)>").

:- lambda_const_add('IsZero',"NIsZero").
:- lambda_const_add('Value',"lmn.nm").

:- lambda_const_add('Svar',"ln.<0N|n>").
:- lambda_const_add('Sapp',"lab.<1N|<a|b>>").
:- lambda_const_add('Slam',"lvw.<2N|<v|w>>").
%:- lambda_const_add('MAP',"Y(lfdi.(Neqi(dTRUETRUE))(dTRUE[])(f(d[])i))").
:- lambda_add_fix_close('NMap','PNMap',"lfdi.(IsNild)<[]|i>((Neqi(dTRUETRUE))<TRUE|dTRUE[]>(f(d[])i))").
:- lambda_add_close_Y('EMap',"ly.(le.y(lfdi.(IsNild)<[]|i>((ei(dTRUETRUE))<TRUE|dTRUE[]>(f(d[])i))))").
:- lambda_add_fix_close('NMember','PNMember',"lfdi.(IsNild)[]((Neqi(dTRUE))TRUE(f(d[])i))").
:- lambda_add_close_Y('EMember',"ly.(le.y(lfdi.(IsNild)[]((ei(dTRUE))TRUE(f(d[])i))))").
%:- lambda_const_add('INT',"Y(lfdi.(Neq0(iTRUE))(MAPd(i[]))((Neq1(iTRUE))((fd(i[]TRUE))(fd(i[][])))(lx.f[<i[]TRUE|x>|d](i[]TRUE))))").
:- lambda_add_close_Y('RINT',"ly.y(lfdi.((yPNMap)[<0|((yPNMap)d(i[]))[]>,<1|(fd(i[]TRUE))(fd(i[][]))>,<2|lx.f[<i[]TRUE|x>|d](i[][])>](iTRUE))[])").

:- lambda_const_add('INT','RINT[]').

:- compile_predicates([lambda_const/2,lam_i_macros//1]).


/**@test (Y(lfx.(NIsZerox)12(f(Npredx))))1
 *
 */

/**@test INT[<1|x>](Svar(1))
 * x
 *
 * @test INT[<1|x>]((Sapp)(Svar1)(Svar1))
 * xx
 *
 * @test INT[](Slam1(Sapp(Svar1)(Svar1)))
 * la.aa
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


nazwa_zmien(z,'#1'):-!.
nazwa_zmien(A,N):-
    atom_codes(A,[C]),!,
    between(97,121,C),
    succ(C,CC),
    atom_codes(N,[CC]).
nazwa_zmien(A,C):-
    atom_codes(A,[_|AA]),
    number_codes(N,AA),
    succ(N,NN),
    atom_concat('#',NN,C).

next_color(256,256):-!.
next_color(N,NN):-
    succ(N,NN).








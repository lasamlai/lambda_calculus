:- module(semantic, [
              string_lambda/2,
              lambda_enum/4,
              lambda_n/2,
              lambda_const/2,
              lambda_eq/4,
              num_bite/2
          ]).

:- use_module(parser).
:- use_module(reduction).

string_lambda(S,LL):-
    string_codes(S,CC),
    codes_lambda(CC,LL).

codes_lambda(CC,LL):-
    phrase(lam(L),CC),!,
    lam_var(L,LL,[],[]).

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
lam_var(enum(Z,K),L,P,P):-
    !,
    lambda_enum(L,Z,K,g).
lam_var(bite(Z,K),L,P,P):-
    !,
    num_bite_p(L,Z,K).
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
    parser:assertz(EE).

:- volatile lambda_const_add_/2.

lambda_const_add_(Atom,Lambda):-
    find_(Lambda,M,[]),
    list_to_and(M,F),
    assertz(:-(lambda_const(Atom,Lambda),F)),
    atom_string(Atom,SS),
    expand_term(lam_i_macros(Atom)-->SS,EE),
    parser:assertz(EE).

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

lambda_enum(L,Z,K,C):-
    lambda_enum_1(L,Z,K,C).

lambda_enum_1(V,_,_,p):-
    var(V),!,fail.
lambda_enum_1(l([A|H]-HH,R),0,KK,C):-
    (   C = g
    ->  H = HH
    ;   H == HH),
    !,
    lsucc(K,KK),
    lambda_enum_2(A,R,K,C).
lambda_enum_1(l(H-HH,R),ZZ,KK,C):-
    (   C = g
    ->  H = HH
    ;   H == HH),
    lsucc(K,KK),
    lsucc(Z,ZZ),
    lambda_enum_1(R,Z,K,C).

lambda_enum_2(A,AA,0,g):-
    A = AA,!.
lambda_enum_2(A,AA,0,p):-
    A == AA,!.
lambda_enum_2(A,l(H-H,R), KK,C):-
    lsucc(K,KK),
    lambda_enum_2(A,R,K,C).

lsucc(N,M):-
    when((nonvar(N);nonvar(M)),system:succ(N,M)).

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

num_bite(L,B):-
    num_bite_(L, 0, B).
num_bite_([], A, A).
num_bite_([H|T], W, L) :-
    lambda_const('TRUE', H),
    WW is W * 2 + 1,
    num_bite_(T, WW, L).
num_bite_([H|T], W, L) :-
    lambda_const('FALSE', H),
    WW is W * 2,
    num_bite_(T, WW, L).

num_bite_p(F,B,Z):-
    num_bite_p_(B, [], L, Z),
    lam_var(krot(L),F,[],[]).
num_bite_p_(0, T, L, Z) :-
    !,
    num_bite_p_zeros(Z, T, L).
num_bite_p_(W, T, L, Z) :-
    1 is W mod 2,
    !,
    WW is W div 2,
    succ(ZZ, Z),
    num_bite_p_(WW, ['TRUE'|T], L, ZZ).
num_bite_p_(W, T, L, Z) :-
    WW is W div 2,
    succ(ZZ, Z),
    num_bite_p_(WW, ['FALSE'|T], L, ZZ).

num_bite_p_zeros(0, L, L) :- !.
num_bite_p_zeros(Z, T, L) :-
    succ(ZZ, Z),
    num_bite_p_zeros(ZZ, ['FALSE'|T], L).


:- dynamic lambda_const/2.

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
:- lambda_const_add('Nmul',"lmnf.m(nf)").
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

:- compile_predicates([lambda_const/2,parser:lam_i_macros//1]).


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
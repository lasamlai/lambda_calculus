:- module(semantic, [
              string_lambda/2,
              lambda_enum/4,
              lambda_n/2,
              lambda_const/2,
              lambda_eq/2,
              num_bite/2
          ]).

:- use_module(parser).
:- use_module(reduction).

string_lambda(S,LL):-
    string_codes(S,CC),
    codes_lambda(CC,LL).

codes_lambda(CC,LL):-
    phrase(lam(L),CC),!,
    lam_var(L,LL,[]).

% Tu Analizujemu sparsowane dane


lam_var(nat(N),L,_):-
    !,
    lambda_number(L,N).
lam_var(int(N,M),L,_):-
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
lam_var(enum(Z,K),L,_):-
    !,
    lambda_enum(L,Z,K,g).
lam_var(bite(Z,K),L,_):-
    !,
    num_bite_p(L,Z,K).
lam_var(pair(V,W),l(B, a(a(B, X), Y)),R):-
    !,
    lam_var(V,X,R),
    lam_var(W,Y,R).
lam_var(krot(KK),l(F, W),R):-
    !,
    reverse(KK,K),
    lam_var_krot(K,F,W,R).
lam_var(list(LL),L,R):-
    !,
    lam_var_list(LL,L,R).
lam_var(A,L,_):-
    atom(A),
    lambda_const(A,L),!.
/*lam_var('Y',l([G, C|A]-A, a(l([E, D|B]-B, a(C, a(D, E))), l([I, H|F]-F, a(G, a(H, I))))),P,P):-
    !.*/
lam_var(l(A,B),l(M,D),R):-
    !,
    lam_var(B,D,[A=M|R]).
lam_var(a(A,B),a(C,D),R):-
    !,
    lam_var(A,C,R),
    lam_var(B,D,R).
lam_var(A,W,R):-
    member(A=W,R),
    !.
lam_var(A,A,_).

lam_var_krot([],F,F,_):-!.
lam_var_krot([A|B],F,a(W,AA),R):-
    lam_var(A,AA,R),
    lam_var_krot(B,F,W,R).

lam_var_list([],L,_):-
    lambda_const('FALSE',L),!.
lam_var_list([A|B],L,R):-
    !,
    lam_var(A,AA,R),
    get_lambda_pair(L,AA,K),
    lam_var_list(B,K,R).
lam_var_list(A,AA,R):-
    lam_var(A,AA,R).

% Consts

:- volatile lambda_add_close_Y/2.

lambda_add_close_Y(Atom,String):-
    string_lambda(String,L),
    fix_reduction(L,LL),
    lambda_const('Y',Y),!,
    reduction(f([],a(LL,Y)),f([],Lambda)),!,
    lambda_const_add_(Atom,Lambda).

:- volatile lambda_add_fix_close/3.

lambda_add_fix_close(YAtom,Atom,String):-
    string_lambda(String,L),
    fix_reduction(L,LL),
    lambda_const_add_(Atom,LL),
    lambda_const('Y',Y),!,
    lambda_const_add_(YAtom,a(Y,LL)).

:- volatile lambda_add_close/2.

lambda_add_close(Atom,String):-
    string_lambda(String,L),
    fix_reduction(L,LL),
    lambda_const_add_(Atom,LL).

:- volatile lambda_const_add/2.

lambda_const_add(Atom,String):-
    string_lambda(String,L),
    lambda_const_add_(Atom, L).

:- volatile lambda_const_add_/2.

lambda_const_add_(Atom,Lambda):-
    assertz(lambda_const(Atom,Lambda)),
    atom_string(Atom,SS),
    expand_term(lam_i_macros(Atom)-->SS,EE),
    parser:assertz(EE).

get_lambda_pair(l(B, a(a(B,X),Y)),X,Y).


lambda_n(l(F,H),N):-
    lambda_n1(F,H,N).
lambda_n1(F,l(X,A),N):-
    lambda_n2(F,X,A,N).
lambda_n2(_,X,A,0):-
    X == A,
    !.
lambda_n2(F1,X,a(F2,A),N):-
    F1 == F2,
    (number(N)->succ(NN,N);true),
    lambda_n2(F1,X,A,NN),
    succ(NN,N).

lambda_enum(L,Z,K,C):-
    lambda_enum_1(L,[],Z,K,C).

lambda_enum_1(V,L,_,_,p):-
    var(V),
    L = [],
    !,
    fail.
lambda_enum_1(V,L,Z,K,p):-
    var(V),
    !,
    length(L, K),
    lnth0(N,L,V),
    Z is K - N - 1.
lambda_enum_1(V,L,Z,K,g):-
    length(L, K),
    !,
    N is K - Z - 1,
    nth0(N,L,V).
lambda_enum_1(l(X,R),L,Z,K,C):-
    lambda_enum_1(R,[X|L],Z,K,C).

lnth0(0, [X|_], V):-
    X == V.
lnth0(NN, [_|L], V):-
    lsucc(N, NN),
    lnth0(N, L, V).

lsucc(N,M):-
    when((nonvar(N);nonvar(M)),system:succ(N,M)).

%!  lambda_number(?Lambda,?Number) is det

lambda_number(_,N):-
    \+ var(N),
    \+ number(N),
    !,
    fail.
lambda_number(l(F,l(X,M)),N):-
    !,
    lambda_number_(F,M,X,N).
lambda_number_(_,X,X,0):-
    !.
lambda_number_(F,a(F,M),X,NN):-
    number(NN),
    !,
    succ(N,NN),
    lambda_number_(F,M,X,N).
lambda_number_(F,a(F,M),X,NN):-
    lambda_number_(F,M,X,N),
    succ(N,NN).

num_bite(L,B):-
    num_bite_(L, 0, B).
num_bite_([], A, A).
num_bite_([H|T], W, L) :-
    lambda_const('TRUE', TRUE),
    lambda_eq(H, TRUE),
    WW is W * 2 + 1,
    num_bite_(T, WW, L).
num_bite_([H|T], W, L) :-
    lambda_const('FALSE', FALSE),
    lambda_eq(H, FALSE),
    WW is W * 2,
    num_bite_(T, WW, L).

num_bite_p(F,B,Z):-
    num_bite_p_(B, [], L, Z),
    lam_var(krot(L),F,[]).
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

%!  lambda_eq(Vars1,L1,Vars2,L2) is det

lambda_eq(A, B):-
    atom(A),atom(B),
    !,
    A = B.
lambda_eq(A, B):-
    var(A),
    !,
    var(B),
    A == B.
lambda_eq(_,B):-
    var(B),
    !,
    fail.
lambda_eq(a(A,B),a(C,D)):-
    lambda_eq(A,C),
    lambda_eq(B,D).
lambda_eq(l(F,A),l(G,B)):-
    F = G,
    lambda_eq(A,B).

:- abolish(lambda_const/2).
:- dynamic lambda_const/2.

:- lambda_const_add('<>',"lx.x").
:- lambda_const_add('Y',"lf.(lx.f(xx))(lx.f(xx))").
:- lambda_const_add('O', "lfgx.f (g x)").

:- lambda_const_add('K', "lx_.x").
:- lambda_const_add('S', "lfgx.f x (g x)").

:- lambda_const_add('NINF',"lfx.Yf").

:- lambda_const_add('[]',"l_a.a").
:- lambda_const_add('FALSE',"l_x.x").
:- lambda_const_add('TRUE',"lx_.x").


:- lambda_const_add('NIL', "λcn.n").
:- lambda_const_add('CONS', "λht.λcn.c h (t c n)").
:- lambda_const_add('NONE', "lns.n").
:- lambda_const_add('SOME', "lens.s e").
:- lambda_const_add('NN', "<<>|NIL>").
:- lambda_const_add('FST', "lt.t K").
:- lambda_const_add('SND', "lt.t []").
:- lambda_const_add('SS', "lxp.<CONS x|(FST p) (SND p)>").
:- lambda_const_add('TAIL', "λt.SND (t SS NN)").


:- lambda_const_add('NSUCC',"lnfx.f(nfx)").
:- lambda_const_add('NSUCCC',"lnfx.nf(fx)").
:- lambda_const_add('NPLUS',"lmnfx.mf(nfx)").
:- lambda_const_add('NPRED',"lnfx.n(lgh.h(gf))(lu.x)(lu.u)").
:- lambda_const_add('NMINUS',"lmn.nNPREDm").
:- lambda_const_add('NMUL',"lmnf.m(nf)").
:- lambda_const_add('NEXP',"lmn.nm").
:- lambda_const_add('NISZERO',"ln.n(lx.FALSE)TRUE").

:- lambda_const_add('PAIR',"labf.fab").
:- lambda_const_add('CAR',"<TRUE>").
:- lambda_const_add('CDR',"<[]>").

:- lambda_const_add('AND',"lpq.pqp").
:- lambda_const_add('OR',"lpq.ppq").
:- lambda_const_add('NOT',"lpab.pba").
:- lambda_const_add('NOTT',"lp.p[]TRUE").

:- lambda_add_close('XOR',"lpq.p(NOTq)q").
:- lambda_add_close('XORR',"lpq.p(NOTTq)q").

:- lambda_add_close('LEFT', "lxfr.fx").
:- lambda_add_close('RIGHT', "lxfr.rx").

%:- lambda_add_close('SOME', "left.fe").
%:- lambda_add_close('NONE', "l_x.x").

:- lambda_add_close('OMAP', "lof.o (λe.SOME (f e)) NONE").

:- lambda_add_close('NXOR',"lpq.NOT(XORpq)").
:- lambda_add_close('NXORR',"lpq.NOTT(XORRpq)").

:- lambda_const_add('ISNIL',"lw.w(lhtd.FALSE)TRUE").

:- lambda_add_close('NLEQ',"lmn.NISZERO(NMINUSmn)").
:- lambda_add_close('NEQ',"lmn.AND(NLEQmn)(NLEQnm)").

:- lambda_const_add('NTOZ',"ln.<n|0>").
:- lambda_add_close('ZNEG',"λz.<CDRz|CARz>").
:- lambda_add_fix_close('ONEZERO','PONEZERO',"lfz.NISZERO(CDRz)z(NISZERO(CDRz)z(f<NPRED(CARz)|NPRED(CDRz)>))").

:- lambda_add_close('ZPLUS',"lxy.<NPLUS(CARx)(CARy)|NPLUS(CDRx)(CDRy)>").
:- lambda_add_close('ZMINUS',"lxy.<NPLUS(CARx)(CDRy)|NPLUS(CDRx)(CARy)>").

:- lambda_const_add('ISZERO',"NISZERO").
:- lambda_const_add('VALUE',"lmn.nm").

:- lambda_const_add('SVAR',"ln.<0N|n>").
:- lambda_const_add('SAPP',"lab.<1N|<a|b>>").
:- lambda_const_add('SLAM',"lvw.<2N|<v|w>>").

:- lambda_add_fix_close('MAP', 'PMAP', "lyfw.w (lht_.<f h|y f t>) []").
:- lambda_add_fix_close('FOLDL', 'PFOLDL', "λyfaw.w (λht_.y f (f a h) t) a").
:- lambda_add_fix_close('FOLDR', 'PFOLDR', "λyfwa.w (λht_.f h (y f t a)) a").

:- lambda_add_fix_close('ASSOC', 'PASSOC', "λypw.w (λht_.p (CAR h) (SOME (CDR h)) (y p t)) NONE").

:- lambda_add_fix_close('NMAP','PNMAP',"lfdi.(ISNILd)<[]|i>((NEQi(dTRUE TRUE))<TRUE|dTRUE[]>(f(d[])i))").
:- lambda_add_close_Y('EMAP',"ly.(le.y(lfdi.(ISNILd)<[]|i>((ei(dTRUE TRUE))<TRUE|dTRUE[]>(f(d[])i))))").
:- lambda_add_fix_close('NMEMBER','PNMember',"lfdi.(ISNILd)[]((NEQi(dTRUE))TRUE(f(d[])i))").
:- lambda_add_close_Y('EMEMBER',"ly.(le.y(lfdi.(ISNILd)[]((ei(dTRUE))TRUE(f(d[])i))))").
%:- lambda_const_add('INT',"Y(lfdi.(Neq0(iTRUE))(MAPd(i[]))((Neq1(iTRUE))((fd(i[]TRUE))(fd(i[][])))(lx.f[<i[]TRUE|x>|d](i[]TRUE))))").
:- lambda_add_close_Y('RINT',"ly.y(lfdi.((yPNMAP)[<0|((yPNMAP)d(i[]))[]>,<1|(fd(i[]TRUE))(fd(i[][]))>,<2|lx.f[<i[]TRUE|x>|d](i[][])>](iTRUE))[])").

:- lambda_const_add('INT','RINT[]').

%:- compile_predicates([lambda_const/2]).


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

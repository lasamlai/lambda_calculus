:- module(pritty_print, [
              write_lambda/1
          ]).

:- use_module(semantic).

%!  write_lambda(Lambda) is det

write_lambda(A):-
    \+ \+ write_lambda_(A,a,1),
    format("\e[39m").

write_lambda_(L,Z,R):-
    witch_lambda_const_(L,K),
    !,
    write_lambda_unlam(K,Z,R).

write_lambda_(A,_,_):-
    var(A),
    !,
    format("\e[92m~w",[A]).
write_lambda_(l(L,B),Z,R):-
    \+ \+ unify_with_occurs_check(L, B),
    !,
    format("\e[91mλ\e[90m_"),
    write_lambda_l(B,Z,R).
write_lambda_(l(Z,B),Z,R):-
    !,
    format("\e[91mλ\e[34m~w",[Z]),
    nazwa_zmien(Z,ZZ),
    write_lambda_l(B,ZZ,R).

write_lambda_(a(A,a(B,C)),Z,R):-
    !,
    write_lambda_n(A,Z,R),
    bracket(write_lambda_(a(B,C),Z,RR),R,RR).

write_lambda_(a(A,C),Z,R):-
    !,
    write_lambda_n(A,Z,R),
    format(" "),
    write_lambda_(C,Z,R).

write_lambda_(A,_,_):-
    format("\e[92m~w",[A]).

% Bracket if it is lambda

write_lambda_n(l(K,W),Z,R):-
    !,
    bracket(write_lambda_(l(K,W),Z,RR), R, RR).

write_lambda_n(A,Z,R):-
    write_lambda_(A,Z,R).

write_lambda_l(L,Z,R):-
    witch_lambda_const_(L,K),!,
    format("\e[91m."),
    write_lambda_unlam(K,Z,R).
write_lambda_l(l(L,B),Z,R):-
    \+ \+ unify_with_occurs_check(L, B),
    !,
    format("\e[90m_"),
    write_lambda_l(B,Z,R).
write_lambda_l(l(Z,B),Z,R):-
    !,
    nazwa_zmien(Z,ZZ),
    format("\e[34m~w",[Z]),
    write_lambda_l(B,ZZ,R).
write_lambda_l(A,Z,R):-
    format("\e[91m."),
    write_lambda_(A,Z,R).

write_lambda_krot([],_,_):-
    format("\e[93m>").
write_lambda_krot([A|B],Z,R):-
    format("\e[93m|"),
    write_lambda_(A,Z,R),
    write_lambda_krot(B,Z,R).

write_lambda_list([],_,_):-
    !,
    format("\e[93m]").
write_lambda_list([A|B],Z,R):-
    !,
    format("\e[93m,"),
    write_lambda_(A,Z,R),
    write_lambda_list(B,Z,R).
write_lambda_list(A,Z,R):-
    !,
    format("\e[93m|"),
    write_lambda_(A,Z,R),
    format("\e[93m]").

witch_lambda_const_(L,const(K)):-
    lambda_const(K,LL),
    lambda_eq(L,LL).
witch_lambda_const_(L,nat(N)):-
    lambda_n(L,N).
witch_lambda_const_(L,int(Z,K)):-
    is_lambda_pair(L,A,B),
    lambda_n(A,N),
    lambda_n(B,M),!,
    Z is N - M,
    K is min(N,M).
witch_lambda_const_(L, enum(Z,K)):-
    lambda_enum(L,Z,K,p).
witch_lambda_const_(L, bite(N,B)):-
    is_lambda_krot(L,K),
    length(K,B),
    num_bite(K,N).
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
    format("\e[93m~w",[K]).
write_lambda_unlam(nat(N),_,_):-
    !,
    format("\e[36m~dN\e[39m",[N]).
write_lambda_unlam(int(N,0),_,_):-
    !,
    format("\e[36m~dZ\e[39m",[N]).
write_lambda_unlam(int(N,K),_,_):-
    !,
    format("\e[36m~dZ~d\e[39m",[N,K]).
write_lambda_unlam(enum(N,K),_,_):-
    !,
    format("\e[36m~dE~d\e[39m",[N,K]).
write_lambda_unlam(bite(N,K),_,_):-
    !,
    format("\e[36m~dB~d\e[39m",[N,K]).

write_lambda_unlam(list([X|Y]),Z,R):-
    !,
    format("\e[93m["),
    write_lambda_(X,Z,R),
    write_lambda_list(Y,Z,R).
write_lambda_unlam(pair(X,Y),A,R):-
    !,
    format("\e[93m<"),
    write_lambda_(X,A,R),
    format("\e[93m|"),
    write_lambda_(Y,A,R),
    format("\e[93m>").
write_lambda_unlam(krot(K),Z,R):-
    !,
    (   K = []
    ->  format("\e[93m<>")
    ;   K = [A|B],
        format("\e[93m<"),
        write_lambda_(A,Z,R),
        write_lambda_krot(B,Z,R)).

is_lambda_pair(l(B, a(a(B,X),Y)),X, Y):-
    \+ \+ unify_with_occurs_check(B, (X, Y)).

is_lambda_krot(l(F, W),M):-
    lambda_krot_(F,W,[],M).

lambda_krot_(F,FF,A,A):-
    F == FF,!.
lambda_krot_(F,a(W,X),L,M):-
    \+ \+ unify_with_occurs_check(F, X),
    lambda_krot_(F,W,[X|L],M).

bracket(F, R, RR):-
    !,
    format("\e[38;5;~dm(",[R]),
    next_color(R,RR),
    F,
    format("\e[38;5;~dm)",[R]).

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

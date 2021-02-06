:- module(parser, [
              lam//1
          ]).


:- use_module(library(dcg/basics)).

% Parsing

lam(L) --> whites,exp(L).

exp(W) --> exp_(A),exp(A,W).

exp(P,W) --> whites,(exp_(A),exp(a(P,A),W);{W=P}).

exp_(E) --> l_exp(E).
exp_(E) --> l_var(E).
exp_(E) --> macro(E).
exp_(E) --> "(",lam(E),")".

l_exp(L) --> l, l_exp_(L).

l --> "l", !.
l --> "λ", !.
l --> "\\".

l_exp_(l(V,E)) --> l_lvar(V), l_exp_(E).
l_exp_(E) --> ".", exp(E).


macro(N) --> num_exp(N).
macro('write') --> "write".
macro('put_char') --> "put_char".
%macro(A) --> lam_i_macros(A).
macro(A) --> up_chars(A).
macro(pair(A,B)) --> "<",lam(A),",",lam(B),">".
macro(krot([])) --> "<>".
macro(krot([A|B])) --> "<",lam(A),lam_i_krot(B).
macro(list([])) --> "[]".
macro(list([A|B])) --> "[",lam(A),lam_i_list(B).

num_exp(nat(N)) --> integer(N),"N".
num_exp(int(N,M)) --> integer(N),"Z",integer(M).
num_exp(enum(N,M)) --> integer(N),"E",integer(M).
num_exp(bite(N,M)) --> integer(N),"B",integer(M).
num_exp(int(N,0)) --> integer(N),"Z".
num_exp(nat(N)) --> integer(N).

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

l_lvar([]) --> "_",!.
l_lvar(A) --> l_var(A).

l_var(A) --> [C],{between(97,122,C),!,atom_codes(A,[C])}.

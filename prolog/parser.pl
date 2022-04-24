:- module(parser, [
              lam//1
          ]).


:- use_module(library(dcg/basics)).

% Parsing

lam(L) --> whites, exp(L).

exp(E) --> app(A), !, (app_last(L), !, {E = a(A, L)}; {E = A}).
exp(L) --> app_last(L).

app(W) --> ato(A), app(A,W).
app(P,W) --> whites, (ato(A), !, app(a(P,A),W); {W=P}).

app_last(E) --> l_exp(E), !.
app_last(E) --> ato(E).

ato(E) --> "(", !, lam(E), ")".
ato(E) --> l_var(E), !.
ato(E) --> macro(E).

l_exp(L) --> l, l_exp_(L).

l --> "l", !.
l --> "λ", !.
l --> "\\".

l_exp_(E) --> ".", !, exp(E).
l_exp_(l(V,E)) --> l_lvar(V), l_exp_(E).

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

l_var(A) --> [C], {between(97,122,C), C \= 108, !, atom_codes(A, [C])}.

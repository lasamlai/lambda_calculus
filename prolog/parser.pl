:- module(parser, [
              lam//1,
              lam_i_macros//1
          ]).

:- dynamic lam_i_macros//1.

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

lam_t(T) --> lam_i(A),whites,(lam_t(A,T);{T=A}).

%```
%lam_t2 :=
%        | lam_i
%        | lam_i lam_t2
%```

lam_t(A,T) --> lam_i(B),whites,(lam_t(a(A,B),T);{T=a(A,B)}).

%```
%lam_i :=
%       | number
%       | "Y"
%       | "(" lam ")"
%       | l_var
%```

lam_i(nat(N)) --> integer(N),"N".
lam_i(int(N,M)) --> integer(N),"Z",integer(M).
lam_i(enum(N,M)) --> integer(N),"E",integer(M).
lam_i(bite(N,M)) --> integer(N),"B",integer(M).
lam_i(int(N,0)) --> integer(N),"Z".
lam_i(nat(N)) --> integer(N).
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


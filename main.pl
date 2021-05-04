:- [codigo_comum, puzzles_publicos].
%--------------Help-Commands-------------------
load:-
    [main].
exit:-
    halt.
%----------------------------------------------
lista_soma([Head|Tail],Sum):-
    lista_soma(Tail,M) , Sum is M + Head.
lista_soma([],0).

notmember(List,X):-
    \+ member(X,List).
%-------------------------------------------------------------------------------
getBetween(Start,End,List,Res):-
    X is Start + 1,
    Y is End - 1,
    bagof(Member,Ele^(between(X,Y,Ele),nth0(Ele,List,Member)),Res).
%-------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):- % 3.1.1
     findall(Res, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum), Combs ).

%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- %3.1.2
     findall(Res1, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum, permutation(Res,Res1) ), Res2 ),
     sort(Res2,Perms).

%-------------------------------------------------------------------------------

espaco_fila(Lista,Esp,H_V):-
    espaco_fila(Lista,Esp,H_V,_,[]).

espaco_fila([],espaco(Number,List),_,Number,List).

espaco_fila([ Head |_],espaco(Number,List),_,Number,List):-
    nonvar(Number),
    nonvar(Head).    

espaco_fila([[_,V]|Tail],Esp,v,_,_):-
    V \= 0,
    espaco_fila(Tail,Esp,v,V,[]).

espaco_fila([[H,_]|Tail],Esp,h,_,_):-
    H \= 0,
    espaco_fila(Tail,Esp,h,H,[]).

espaco_fila([[_,V]|Tail],Esp,v,_,_):-
    V==0,
    espaco_fila(Tail,Esp,v,_,_).

espaco_fila([[H,_]|Tail],Esp,h,_,_):-
    H==0,
    espaco_fila(Tail,Esp,h,_,_).

espaco_fila([Head|Tail],Esp,H_V,Number,List):-
    var(Head),
    append(List,[Head],Y),
    espaco_fila(Tail,Esp,H_V,Number,Y).

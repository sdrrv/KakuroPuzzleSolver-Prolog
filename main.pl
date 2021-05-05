:- [codigo_comum, puzzles_publicos].
%--------------Help-Commands-------------------
load:-
    [main].
exit:-
    halt.
%---------------Aux-Commands-------------------
lista_soma([Head|Tail],Sum):- %Returns the sum of all the elements in a given list.
    lista_soma(Tail,M) , Sum is M + Head.
lista_soma([],0).

notmember(List,X):- %Returns True if X is not in the Lists.
    \+ member(X,List).

getBetween(Start,End,List,Res):- 
    X is Start + 1,
    Y is End - 1,
    bagof(Member,Ele^(between(X,Y,Ele),nth0(Ele,List,Member)),Res).

concat(List,Res):-
    concat(List,Res0,[]),
    reverse(Res0,Res).

concat([],Res,Res).
concat([Head|Tail],Res,Aux):-
    append(Head,Aux,Y),
    concat(Tail,Res,Y).

var_in_espaco(espaco(_,Espacos),Var):-
    var_in_espaco(Espacos,Var).

var_in_espaco([Head|_],Var):-
    Head == Var,!.
var_in_espaco([_|Tail],Var):-
    var_in_espaco(Tail,Var).

%-------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):- % 3.1.1
     findall(Res, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum), Combs ).

%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- %3.1.2
     findall(Res1, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum, permutation(Res,Res1) ), Res2 ),
     sort(Res2,Perms).

%-----------------------------------3.1.3----------------------------------------
espaco_fila(Lista,Esp,H_V):-
    espaco_fila(Lista,Esp,H_V,_,[]).

espaco_fila([],espaco(Number,List),_,Number,List):-
    nonvar(Number).

espaco_fila([ Head |_],espaco(Number,List),_,Number,List):-
    nonvar(Number),
    nonvar(Head).    

espaco_fila([[_,H]|Tail],Esp,h,_,_):-
    H \= 0,
    espaco_fila(Tail,Esp,h,H,[]).

espaco_fila([[V,_]|Tail],Esp,v,_,_):-
    V \= 0,
    espaco_fila(Tail,Esp,v,V,[]).

espaco_fila([[_,H]|Tail],Esp,h,_,_):-
    H==0,
    espaco_fila(Tail,Esp,h,_,_).

espaco_fila([[V,_]|Tail],Esp,v,_,_):-
    V==0,
    espaco_fila(Tail,Esp,v,_,_).

espaco_fila([Head|Tail],Esp,H_V,Number,List):-
    var(Head),
    append(List,[Head],Y),
    espaco_fila(Tail,Esp,H_V,Number,Y).
%-------------------------------------------------------------

espacos_fila(H_V, Fila, Espacos):- %3.1.4
    bagof(Esp,(espaco_fila(Fila,Esp,H_V)),Espacos).

%-------------------------3.1.5--------------------------------

espacos_puzzle(Puzzle,Espacos):-
    espacos_puzzle_aux(Puzzle,h,Res01),
    mat_transposta(Puzzle, Trans),
    espacos_puzzle_aux(Trans,v,Res02),
    append(Res01, Res02, Espacos).

espacos_puzzle_aux(Puzzle,H_V,Res):- 
    bagof(Espaco,Fila^(member(Fila, Puzzle), espacos_fila(H_V, Fila, Espaco)), Res0),
    concat(Res0,Res).

%--------------------------3.1.6----------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
    !.












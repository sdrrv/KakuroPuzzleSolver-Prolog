:- [codigo_comum].
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


concat(List,Res):-
    concat(List,Res,[]).

concat([],Res,Res).
concat([Head|Tail],Res,Aux):-
    append(Aux,Head,Y),
    concat(Tail,Res,Y).


var_in_espaco(espaco(_,Espacos),Var):-
    var_in_espaco(Espacos,Var).

var_in_espaco([Head|_],Var):-
    Head == Var,!.
var_in_espaco([_|Tail],Var):-
    var_in_espaco(Tail,Var).


is_same_espaco(espaco(_,Esp),List):-
    Esp == List.


is_same_espaco_V2(espaco(_,Esp1),espaco(_,Esp2)):-
    Esp1 == Esp2.


get_soma_espaco(espaco(Soma,_),Soma).


get_lenght_espaco(espaco(_,Lista), Lenght):-
    length(Lista, Lenght).


get_list_espaco(espaco(_,List),List).


find_var(Index,List,El):- % will return the index of a give var in a given list
    find_var(Index,List,El,0).

find_var(Aux,[Head|_],El,Aux):-
    Head == El,!.
find_var(Index,[_|Tail],El,Aux):-
    Y is Aux + 1,
    find_var(Index,Tail,El,Y).


unifica([],_):-
    !.
unifica(_,[]):-
    !.

unifica([Head1|Tail1], [_|Tail2]):-
    var(Head1),!,
    unifica(Tail1,Tail2).

unifica([Head1|Tail1], [Head2|Tail2]):-
    Head1 == Head2,!,
    unifica(Tail1,Tail2).

get_permPoss_length([_,PermPoss],Res):-
    length(PermPoss,Res).

%-------------------------------------------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):- % 3.1.1
     findall(Res, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum), Combs ).

%-------------------------------------------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- %3.1.2
     findall(Res1, (combinacao(N, Els, Res), lista_soma(Res,Sum), Soma == Sum, permutation(Res,Res1) ), Res2 ),
     sort(Res2,Perms).

%-----------------------------------3.1.3----------------------------------------
espaco_fila(Lista,Esp,H_V):-
    espaco_fila(Lista,Esp,H_V,_,[]),
    get_lenght_espaco(Esp,Length),
    Length \= 0.
    
espaco_fila([],espaco(Number,List),_,Number,List):-
    nonvar(Number).

espaco_fila([ Head |_],espaco(Number,List),_,Number,List):-
    nonvar(Number),
    nonvar(Head).    

espaco_fila([[_,H]|Tail],Esp,h,_,_):-
    nonvar(H),
    espaco_fila(Tail,Esp,h,H,[]).

espaco_fila([[V,_]|Tail],Esp,v,_,_):-
    nonvar(V),
    espaco_fila(Tail,Esp,v,V,[]).

espaco_fila([Head|Tail],Esp,H_V,Number,List):-
    var(Head),
    append(List,[Head],Y),
    espaco_fila(Tail,Esp,H_V,Number,Y).
%-------------------------------------------------------------

espacos_fila(H_V, Fila, Espacos):- %3.1.4
    bagof(Esp,(espaco_fila(Fila,Esp,H_V)),Espacos),!.
espacos_fila(_,_,[]).

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

espacos_com_posicoes_comuns(Espacos, espaco(_,Esp), Esps_com):-
    bagof(Espaco, Main^(member(Espaco, Espacos), member(Main, Esp), var_in_espaco(Espaco, Main), \+ is_same_espaco(Espaco,Esp) ), Esps_com).

%--------------------------3.1.7----------------------------------

permutacoes_soma_espacos(Espacos, Perms_soma):-
    reverse(Espacos,X),
    permutacoes_soma_espacos(X,[],Perms_soma).

permutacoes_soma_espacos([Head|Tail],Aux,Perms_soma):-
    get_soma_espaco(Head,Sum),
    get_lenght_espaco(Head,Lenght),
    permutacoes_soma(Lenght,[1,2,3,4,5,6,7,8,9],Sum,X),
    append([[Head,X]],Aux,Y ),
    permutacoes_soma_espacos(Tail,Y,Perms_soma).
permutacoes_soma_espacos([],Aux,Aux).

%--------------------------3.1.8----------------------------------

get_PermsSoma(Esp,Perms_soma,Res):- %Returns the Perms_soma of the given Espaco
    member(X,Perms_soma),
    nth0(0,X,Esp1),
    is_same_espaco_V2(Esp,Esp1),!,
    nth0(1,X,Res).

%permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
   % get_PermsSoma(Esp,Perms_soma,PermsS),
    %get_list_espaco(Esp,EspList),
   % espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
   % permutacao_possivel_espaco_aux(EspList,PermsS,Esps_com,Perms_soma,Perm).

%permutacao_possivel_espaco_aux(EspList,EspPerms,Esps_com,Perms_soma,Perm):-
    %fail.


%--------------------------3.1.9----------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss):-
    get_list_espaco(Esp,EspList),
    permutacao_possivel_espaco(Perm,Esp,Espacos,Perms_soma),
    append([EspList], Perm, Perms_poss).

%--------------------------3.1.10---------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss, Esp^( member(Esp,Espacos),
    permutacoes_possivel_espaco(Espacos,Perms_soma,Esp,Perms_poss)), Perms_poss_esps ).

%-------------------------3.1.11-----------------------------------
    
numeros_comuns(Lst_Perms, Numeros_comuns):-
    nth1(1,Lst_Perms,First),
    numeros_comuns(Lst_Perms, Res0, [],1, First),
    reverse(Res0,Numeros_comuns).
    

numeros_comuns(_, AuxList, AuxList, AuxNum,First):-
    length(First,X),
    AuxNum is X +1,!.

numeros_comuns(Lst_Perms, Numeros_comuns, AuxList, AuxNum, First):-
    nth1(AuxNum, First,Current),
    findall(El,( member(X,Lst_Perms), 
             nth1(AuxNum,X,El),
             nth1(AuxNum, First,Current),
             El == Current), Tmp),
    length(Lst_Perms,Lng),
    length(Tmp,Lng2),
    numeros_comuns_aux(Lng,Lng2,AuxNum,AuxList,Current,Z),
    Y is AuxNum + 1,
    numeros_comuns(Lst_Perms, Numeros_comuns, Z, Y, First).

numeros_comuns_aux(Lng1,Lng2,AuxNum,AuxList,Current,Res):-
    Lng1 == Lng2,!,
    append([(AuxNum,Current)], AuxList, Res).

numeros_comuns_aux(_,_,_,AuxList,_,AuxList).

%-------------------------3.1.12-----------------------------------

atribui_comuns([]):-
    !.
atribui_comuns([Head|Tail]):-
    nth0(1,Head,Lst_Perms),
    numeros_comuns(Lst_Perms,Numeros_comuns),
    nth0(0,Head,PermList),
    atribui_comuns_apply(PermList,Numeros_comuns),
    atribui_comuns(Tail).

atribui_comuns_apply(_,[]):-
    !.
atribui_comuns_apply(PermList,[(Index,Replacer)|Tail]):-
    nth1(Index,PermList,Replacer),
    atribui_comuns_apply(PermList,Tail).

%-------------------------3.1.13-----------------------------------

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):-
    bagof(Res2,(retira_impossiveis_bagof_unify(Perms_Possiveis, Res2))
          ,Novas_Perms_Possiveis).

retira_impossiveis_bagof_unify(Perms_Possiveis, Res2):-
    member(Perms,Perms_Possiveis),
    nth0(0, Perms, PermsList),
    nth0(1, Perms, WorkList),
    retira_impossiveis_aux(PermsList,WorkList,Res),
    append([PermsList],[Res], Res2).

retira_impossiveis_aux(PermsList,WorkList,Res):-
    bagof(Possibility,Possibility^(member(Possibility, WorkList),
    unifica(PermsList,Possibility)),Res).

%-------------------------3.1.14-----------------------------------

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis).

%-------------------------3.1.15-----------------------------------

inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
    simplifica(Perms_poss_esps, Perms_Possiveis).

%------------------------------3.2.1-----------------------------------

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha, 0, _).

escolhe_menos_alternativas([],AuxEsp,_,AuxEsp):-
    var(AuxEsp),fail,!; nonvar(AuxEsp),!.

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, AuxEsp):-
    get_permPoss_length(Head,Len),
    Len =< 1,!,
    escolhe_menos_alternativas(Tail, Escolha, AuxLength, AuxEsp).

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, _):-
    AuxLength == 0,
    get_permPoss_length(Head,Len),
    Len >= 1,!,
    escolhe_menos_alternativas(Tail, Escolha, Len, Head).

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, AuxEsp):-
    get_permPoss_length(Head,Len),
    Len >= AuxLength,!,
    escolhe_menos_alternativas(Tail, Escolha, AuxLength, AuxEsp).

escolhe_menos_alternativas([Head|Tail], Escolha, _, _):-
    get_permPoss_length(Head,Len),
    escolhe_menos_alternativas(Tail, Escolha, Len, Head).

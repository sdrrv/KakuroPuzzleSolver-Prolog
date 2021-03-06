/*
---------------------------------------------------------------------------
        
                            Logica de Programacao
                                Duarte Elvas
                                   198564
                                     TG

----------------------------------------------------------------------------
*/
:- [codigo_comum].
%:- [puzzles_publicos].
/*--------------Help-Commands-------------------
    Criados para ajudar no debug do codigo     */

    load:-  
    [main].
exit:-
    halt.

%---------------Aux-Commands-------------------
/*

        Comandos Auxiliares que auxiliam os predicados principais

*/

lista_soma([Head|Tail],Sum):- %Devolve a soma de todos os elementos numa dada lista
    lista_soma(Tail,M) , Sum is M + Head.
lista_soma([],0).


notmember(List,X):- %E verdadeiro se o X nao estiver na Lista "List"
    \+ member(X,List).


concat(List,Res):-% Se existir agulguma lista [[1]], ele vai retirar a lita de dentro deixando [1].
    concat(List,Res,[]).

concat([],Res,Res). %Condicao de saida
concat([Head|Tail],Res,Aux):- %Loop recursivo
    append(Aux,Head,Y),
    concat(Tail,Res,Y).


var_in_espaco(espaco(_,Espacos),Var):-% Vai verificar se a Variavel dada esta contida na lista do espaco dado
    var_in_espaco(Espacos,Var).

var_in_espaco([Head|_],Var):-% Condicao de saida
    Head == Var,!.
var_in_espaco([_|Tail],Var):-% Loop Recursivo
    var_in_espaco(Tail,Var).


is_same_espaco(espaco(_,Esp),List):-% Verifica se a Lista dada e a mesma lista do espaco dado.
    Esp == List.


is_same_espaco_V2(espaco(_,Esp1),espaco(_,Esp2)):-% Verifica se dois espacos sao iguais, ou seja, os mesmos.
    Esp1 == Esp2.


get_soma_espaco(espaco(Soma,_),Soma).% Devolve o valor da Soma de um dado espaco.


get_lenght_espaco(espaco(_,Lista), Lenght):-% Devolve o tamanho da lista de variaveis de um dado espaco.
    length(Lista, Lenght).


get_list_espaco(espaco(_,List),List).% Devolve a lista de variaveis de um determinado espaco


find_var(Index,List,El):- % Vai devolver o index da Variavel dada, na Lista dada: [X,Y],X -> 1.
    find_var(Index,List,El,1).

find_var(Aux,[Head|_],El,Aux):-% Condicao de saida
    Head == El,!.
find_var(Index,[_|Tail],El,Aux):-% Loop recursivo
    Y is Aux + 1,
    find_var(Index,Tail,El,Y).


% O predicado Unifica vai verificar que duas listas sao unificaveis
unifica([],_):-% Condicao de saida
    !.
unifica(_,[]):-% Condicao de Saida
    !.

unifica([Head1|Tail1], [_|Tail2]):-% Loop recursivo
    var(Head1),!,
    unifica(Tail1,Tail2).

unifica([Head1|Tail1], [Head2|Tail2]):-% Loop recursivo
    Head1 == Head2,!,
    unifica(Tail1,Tail2).


get_permPoss_length([_,PermPoss],Res):-% Vai devolver o tamanho da Lista de Permotacoes possiveis de um dado espaco
    length(PermPoss,Res).

%O predicado Unificar, vai unificar as duas listas dadas
unificar([],[]):-% Condicao de Saida
    !.
unificar([Head1|Tail1], [Head2|Tail2]):-% Loop recursivo se a Head vor variavel
    var(Head1),!,
    Head1 = Head2,
    unificar(Tail1,Tail2).
unificar([_|Tail1], [_|Tail2]):- % Loop recursivo
    unificar(Tail1,Tail2).

/*
------------------------------------------------------------------------------------
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                  
                      !Aqui Comecam os Predicados Avaliados!
                      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
------------------------------------------------------------------------------------
*/

%------------------------------------3.1.1------------------------------------------

combinacoes_soma(N, Els, Soma, Combs):- % Var decolver uma lista com todas as combinacoes em que a soma dos seus membros vai ser igual a "Soma"
     findall(Res, (combinacao(N, Els, Res),
        lista_soma(Res,Sum),
        Soma == Sum), Combs ).

%------------------------------------3.1.2------------------------------------------

permutacoes_soma(N, Els, Soma, Perms):- % Vai devolver uma lista com todas as permutacoes em que a soma dos seus membros vai ser igual a "Soma".
     findall(Res1, (combinacao(N, Els, Res),
        lista_soma(Res,Sum),
        Soma == Sum,
        permutation(Res,Res1) ), Res2 ),
     sort(Res2,Perms).

%-----------------------------------3.1.3----------------------------------------
espaco_fila(Lista,Esp,H_V):- % Vai devolver os espacos(estrutura) numa dada fila (Linha ou Coluna).
    espaco_fila(Lista,Esp,H_V,_,[]),
    get_lenght_espaco(Esp,Length),
    Length \= 0.
    
espaco_fila([],espaco(Number,List),_,Number,List):- % Condicao de saida
    nonvar(Number).

espaco_fila([ Head |_],espaco(Number,List),_,Number,List):-% Condicao de Saida
    nonvar(Number),
    nonvar(Head).    

espaco_fila([[_,H]|Tail],Esp,h,_,_):-% Recursao se for horizontal
    nonvar(H),
    espaco_fila(Tail,Esp,h,H,[]).

espaco_fila([[V,_]|Tail],Esp,v,_,_):-% Recursao de for vertical
    nonvar(V),
    espaco_fila(Tail,Esp,v,V,[]).

espaco_fila([Head|Tail],Esp,H_V,Number,List):-% Recursao
    var(Head),
    append(List,[Head],Y),
    espaco_fila(Tail,Esp,H_V,Number,Y).
%--------------------------3.1.4------------------------------

espacos_fila(H_V, Fila, Espacos):- % Junta todos os espacos de uma determinada fila numa lista.
    bagof(Esp,(espaco_fila(Fila,Esp,H_V)),Espacos),!.
espacos_fila(_,_,[]).% Condicao de saida

%-------------------------3.1.5--------------------------------

espacos_puzzle(Puzzle,Espacos):-% Devolve uma Lista Espacos, com todos os espacos possiveis.
    espacos_puzzle_aux(Puzzle,h,Res01),
    mat_transposta(Puzzle, Trans),
    espacos_puzzle_aux(Trans,v,Res02),
    append(Res01, Res02, Espacos).

espacos_puzzle_aux(Puzzle,H_V,Res):-% Devolve para a funcao principal todos os espacos de uma determinada orientacao. 
    bagof(Espaco,Fila^(member(Fila, Puzzle), 
        espacos_fila(H_V, Fila, Espaco)), Res0),
    concat(Res0,Res).

%--------------------------3.1.6----------------------------------

espacos_com_posicoes_comuns(Espacos, espaco(_,Esp), Esps_com):- % Devolve uma Lista com todos os espacos com posicoes comuns as posicoes do espaco dadp
    bagof(Espaco, Main^(member(Espaco, Espacos), 
        member(Main, Esp), 
        var_in_espaco(Espaco, Main), 
        \+ is_same_espaco(Espaco,Esp) ), Esps_com).

%--------------------------3.1.7----------------------------------

permutacoes_soma_espacos(Espacos, Perms_soma):- % Devolve uma lita com as perutacoes possiveis para cada espaco do puzzle
    reverse(Espacos,X),
    permutacoes_soma_espacos(X,[],Perms_soma).

permutacoes_soma_espacos([Head|Tail],Aux,Perms_soma):% Recursao
    get_soma_espaco(Head,Sum),
    get_lenght_espaco(Head,Lenght),
    permutacoes_soma(Lenght,[1,2,3,4,5,6,7,8,9],Sum,X),
    append([[Head,X]],Aux,Y ),
    permutacoes_soma_espacos(Tail,Y,Perms_soma).
permutacoes_soma_espacos([],Aux,Aux).%Condicao de saida

%--------------------------3.1.8----------------------------------

get_PermsSoma(Esp,Perms_soma,Res):- %Returns the Perms_soma of the given Espaco
    member(X,Perms_soma),
    nth0(0,X,Esp1),
    is_same_espaco_V2(Esp,Esp1),!,
    nth0(1,X,Res).


exists_at_least_one(Index,El,Lists):-% Verifica na lista dada, se existe pelo menos uma lista que tenha no Index "Index" o El. 
    bagof(List,ElList^(member(List,Lists),
                        nth1(Index,List,ElList),
                        ElList == El,!), Res01),
    length(Res01,Len),
    Len >= 1.


divide_espacos_com_posicoes_comuns(EspList,Esps_com,Res):-%Divide numa lista os espacos comuns referentes a cada variavel do espaco dado
    bagof(List,Space^(member(Space,EspList),
                   divide_espacos_com_posicoes_comuns_aux(Space,Esps_com,List)),Res).

divide_espacos_com_posicoes_comuns_aux(Space,Esps_com,Res):-%Cria a sublista com cada espaco
    bagof([Index,Espaco],ListEsp^(member(Espaco,Esps_com), get_list_espaco(Espaco,ListEsp),
                   find_var(Index,ListEsp,Space)),Res).

 permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-% Devolve a permutacao possivel para cada espaco
   get_PermsSoma(Esp,Perms_soma,PermsS),
   get_list_espaco(Esp,EspList),
   espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
   divide_espacos_com_posicoes_comuns(EspList,Esps_com,Esps_com_divided),
   permutacao_possivel_espaco_aux(PermsS,Esps_com_divided,Perms_soma,Perm).

permutacao_possivel_espaco_aux(PermsS,Esps_com_divided,Perms_soma,Perm):-% Verifica todas as pemutacoes
    member(Perm,PermsS),
    permutacao_possivel_espaco_single(Esps_com_divided,Perm,Perms_soma).   
    

permutacao_possivel_espaco_single(Esps_com_divided,EspPerm,Perms_soma):-%Verifica permutacao a permutacao
    length(EspPerm,Len),
    findall(Index,(
        between(1,Len,Index),
        nth1(Index,EspPerm,Value),
        nth1(Index,Esps_com_divided,Esps_com),
        permutacao_possivel_espaco_single_single(Value,Esps_com,Perms_soma)),Res01),
    length(Res01,Res01_len),
    Len == Res01_len.

permutacao_possivel_espaco_single_single(Value,Esps_com,Perms_soma):-%Verifica posicao a posicao
    
    bagof(Esp_com,(member(Esp_com_Matrix, Esps_com),nth1(2,Esp_com_Matrix,Esp_com),
                               nth1(1,Esp_com_Matrix,Esp_com_Index),
                               get_PermsSoma(Esp_com,Perms_soma,Esp_com_Perms),
                               exists_at_least_one(Esp_com_Index,Value,Esp_com_Perms)),Esps_com_checked),
    
    length(Esps_com,Eps_com_len),
    length(Esps_com_checked,Esps_com_checked_len),
    Eps_com_len == Esps_com_checked_len, true.


%--------------------------3.1.9----------------------------------

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss):-%Devolve o espaco com todas as suas permutacoes possiveis
    get_list_espaco(Esp,EspList),
    findall(Perm,(
        permutacao_possivel_espaco(Perm,Esp,Espacos,Perms_soma)),Perms),
        append([EspList], [Perms], Perms_poss).

%--------------------------3.1.10---------------------------------

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-%Devolve uma lista com todos os espacos e as suas respetivas permutacoes possiveis
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(Perms_poss,Esp^( member(Esp,Espacos), 
        permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)),Perms_poss_esps).

%-------------------------3.1.11-----------------------------------
    
numeros_comuns(Lst_Perms, Numeros_comuns):- % devolve uma estrutura com o indice e o inteiro comuns a todas as listas da lista
    nth1(1,Lst_Perms,First),
    numeros_comuns(Lst_Perms, Res0, [],1, First),
    reverse(Res0,Numeros_comuns).
    

numeros_comuns(_, AuxList, AuxList, AuxNum,First):- %condicao de saida
    length(First,X),
    AuxNum is X +1,!.

numeros_comuns(Lst_Perms, Numeros_comuns, AuxList, AuxNum, First):- % loop Recursivo
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

numeros_comuns_aux(Lng1,Lng2,AuxNum,AuxList,Current,Res):- % Prepara o output
    Lng1 == Lng2,!,
    append([(AuxNum,Current)], AuxList, Res).

numeros_comuns_aux(_,_,_,AuxList,_,AuxList).%condicao de saida

%-------------------------3.1.12-----------------------------------

atribui_comuns([]):- %Atribui os numeros comuns ao espaco
    !.
atribui_comuns([Head|Tail]):- %Loop Recursivo
    nth0(1,Head,Lst_Perms),
    numeros_comuns(Lst_Perms,Numeros_comuns),
    nth0(0,Head,PermList),
    atribui_comuns_apply(PermList,Numeros_comuns),
    atribui_comuns(Tail).

atribui_comuns_apply(_,[]):-% Condicao de Saida
    !.
atribui_comuns_apply(PermList,[(Index,Replacer)|Tail]):-%Atribui 
    nth1(Index,PermList,Replacer),
    atribui_comuns_apply(PermList,Tail).

%-------------------------3.1.13-----------------------------------

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):- % Retira os Impossiveis apos a unificacao anterior
    bagof(Res2,(retira_impossiveis_bagof_unify(Perms_Possiveis, Res2))
          ,Novas_Perms_Possiveis).

retira_impossiveis_bagof_unify(Perms_Possiveis, Res2):- %Unifica 
    member(Perms,Perms_Possiveis),
    nth0(0, Perms, PermsList),
    nth0(1, Perms, WorkList),
    retira_impossiveis_aux(PermsList,WorkList,Res),
    append([PermsList],[Res], Res2).

retira_impossiveis_aux(PermsList,WorkList,Res):-%Devolve Unificado
    bagof(Possibility,Possibility^(member(Possibility, WorkList),
        unifica(PermsList,Possibility)),Res).

%-------------------------3.1.14-----------------------------------

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-% Vai simplificar ate nada mudar
    simplifica_aux(Perms_Possiveis, Novas_Perms_Possiveis,_).

    
simplifica_aux(Perms_Possiveis, AuxBag, AuxBag):-%  Condicao de saida
    Perms_Possiveis == AuxBag,!.

simplifica_aux(Perms_Possiveis, Novas_Perms_Possiveis,AuxBag):-% Loop recursivo inicial
    var(AuxBag),!,
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis_local),
    simplifica_aux(Perms_Possiveis, Novas_Perms_Possiveis,Novas_Perms_Possiveis_local).

simplifica_aux(_, Novas_Perms_Possiveis,AuxBag):--% Loop recursivo apos primeiro loop
    atribui_comuns(AuxBag),
    retira_impossiveis(AuxBag, Novas_Perms_Possiveis_local),
    simplifica_aux(AuxBag, Novas_Perms_Possiveis,Novas_Perms_Possiveis_local).

%-------------------------3.1.15-----------------------------------

inicializa(Puzzle, Perms_Possiveis):- %Vai inicializar todo o processo de resolver o puzzle.
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
    simplifica(Perms_poss_esps, Perms_Possiveis).

%------------------------------3.2.1-----------------------------------

escolhe_menos_alternativas(Perms_Possiveis, Escolha):- % vai escolher de entre todos os membros da lita, o elemento que conter o menor numero de permutacoes possiveis se sejam maiores que 1
    escolhe_menos_alternativas(Perms_Possiveis, Escolha, 0, _).

escolhe_menos_alternativas([],AuxEsp,_,AuxEsp):- % condicao de saida
    var(AuxEsp),fail,!; nonvar(AuxEsp),!. 

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, AuxEsp):-%Loop Recursivo
    get_permPoss_length(Head,Len),
    Len =< 1,!,
    escolhe_menos_alternativas(Tail, Escolha, AuxLength, AuxEsp).

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, _):-%Loop Recursivo
    AuxLength == 0,
    get_permPoss_length(Head,Len),
    Len >= 1,!,
    escolhe_menos_alternativas(Tail, Escolha, Len, Head).

escolhe_menos_alternativas([Head|Tail], Escolha, AuxLength, AuxEsp):-%Loop Recursivo
    get_permPoss_length(Head,Len),
    Len >= AuxLength,!,
    escolhe_menos_alternativas(Tail, Escolha, AuxLength, AuxEsp).

escolhe_menos_alternativas([Head|Tail], Escolha, _, _):-%Loop Recursivo
    get_permPoss_length(Head,Len),
    escolhe_menos_alternativas(Tail, Escolha, Len, Head).

%---------------------------------------3.1.2--------------------------------------

replace_permPoss(Perms_Possiveis,Perm,Replacement,Res):-% vai substituir a permutacao dada nas "Perms_Possiveis"
    maplist(replace_permPoss_aux(Perm,Replacement),Perms_Possiveis,Res).

replace_permPoss_aux(Perm,Replacement,El,Replacement):-
    Perm == El,!.

replace_permPoss_aux(_,_,El,El).

experimenta_perm([Esp,Lst_Perms], Perms_Possiveis,Novas_Perms_Possiveis):-% vai esprimentar substituir os membros e verificar se funciona
    member(Perm,Lst_Perms),
    unificar(Esp,Perm),
    append([Esp],[[Perm]],El),
    replace_permPoss(Perms_Possiveis,[Esp,Lst_Perms],El,Novas_Perms_Possiveis).

%--------------------------------------------------------------------------------------

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-% vai resolver recursivamente ate conseguir obter um resultado
    length(Perms_Possiveis,Len),
    resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis,Len).
    
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis,Len):-%Loop Recursivo
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),!,
	experimenta_perm(Escolha, Perms_Possiveis, Res01),
    simplifica(Res01,Res02),
    resolve_aux(Res02, Novas_Perms_Possiveis,Len).
    
resolve_aux(Perms_Possiveis, Perms_Possiveis,Len):-%condicao de saida
    length(Perms_Possiveis,ResLen),
    ResLen == Len,
    !.

%--------------------------------------------------------------------------------------

resolve(Puz):-%Resolve O Puzzle envocando os predicados anteriores
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).

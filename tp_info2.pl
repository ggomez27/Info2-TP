:- use_module(library(clpfd)). % Libreria que contiene transpose/2

%Resuelve un acertijo de Rascacielos, leyendo las pistas con forma de predicado pistas/4 y genera los conjuntos de pistas de filas y columnas
rascacielos(Mat, Fs, Cs) :-
    leerPistas(M,N,L,Fs,Cs),
    length(Mat,M),
    length(MT,N),
    maplist(lstRascacielos(N,L),Fs,Mat), %Aca se crean las instancias de cada elemento
    transpose(Mat,MT), %Se transpone
    maplist(lstRascacielos(M,L),Cs,MT). %Se verifica que se cumpla, no se instancia

%Resuelve un acertijo de Rascacielos, leyendo las pistas con forma de predicado pistas/4
rascacielos(Mat) :-
    rascacielos(Mat,_,_).

leerPistas(M,N,L,Fs,Cs):-
    setof((IFFw,FFw), pista(horizontal,IFFw,izquierda,FFw), FilasForwSet), %Buscamos los pares de Indice y Numero de pista para todas las pistas
    setof((IFBw,FBw), pista(horizontal,IFBw,derecha,FBw), FilasBackSet), %Setof ordena las pistas encontradas e ignora las duplicadas
    setof((ICFw,CFw), pista(vertical,ICFw,arriba,CFw), ColumForwSet),  %Pero no ignora pistas diferentes para una misma fila/columna en mismo sentido
    setof((ICBw,CBw), pista(vertical,ICBw,abajo,CBw), ColumBackSet), %Esto se corrige en rellenarLista(), donde solo toma la primera instancia de una pista
    setof(IF,H^Hw^pista(horizontal,IF,H,Hw), IFs),
    setof(IC,V^Vw^pista(vertical,IC,V,Vw),ICs),
    max_list(IFs, M), %M es la cantidad de filas
    max_list(ICs,N), %N es la cantidad de columnas
    L is max(M,N), %L como el maximo entre M y N dictara el tamaño maximo de edificio
    rellenarLista(M, FilasForwSet, FilasForwSetOrd), %Fw aqui corresponde a las pistas de lado izquierdo
    rellenarLista(M, FilasBackSet, FilasBackSetOrd), %Bw aqui corresponde a las pistas de lado derecho
    rellenarLista(N, ColumBackSet, ColumBackSetOrd), %Fw aqui corresponde a las pistas de lado superior
    rellenarLista(N, ColumForwSet, ColumForwSetOrd), %Bw aqui corresponde a las pistas de lado inferior
    !, %A continuacion se juntan las pistas en pares (Fw,Bw) para las filas y columnas
    juntarPistas(N,FilasForwSetOrd,FilasBackSetOrd, Fs), %Aqui se utiliza N porque son M filas de N elementos
    juntarPistas(M,ColumForwSetOrd,ColumBackSetOrd, Cs). %Aqui se utiliza M porque son N columnas de M elementos

numForwR(_,[],_,0). %Si no quedan edificios, no habra ninguno visible

numForwR(N,[N|_],_,1). %Si el edificio actual es el mas alto posible, solo se vera 1 edificio en este punto

numForwR(N,[H|T],CurMax, Num):- %Si el edificio es mas bajo que alguno de los anteriores, no se suma a los edificios visibles
    H < N,
    H < CurMax,
    numForwR(N,T,CurMax,Num).

numForwR(N,[H|T],CurMax, Num):- %Si el edificio es mas alto que los anteriores, se suma 1 a los edificios visibles
    H < N,
    H > CurMax,
    numForwR(N,T,H,TailNum),
    Num is TailNum + 1.

numForw(N,Lst, Num) :- %Num es el numero de edificios visibles al revisar la lista en sentido derecho (izquierda-derecha o arriba-abajo)
    numForwR(N,Lst, 0, Num), !.

numBack(N,Lst, Num) :- %Num es el numero de edificios visibles al revisar la lista en orden reverso (derecha-izquierda o abajo-arriba)
    reverse(Lst, LR),
    numForw(N,LR, Num).

%Linea de Rascacielos, verifica que para N edificios, de altura hasta L, se cumple el par de pistas NFw y NBw en RLst lista de rascacielos
%Busca o verifica una solucion para una fila o columna
lstRascacielos(N,L,(NFw,NBw),RLst) :- % Hay N edificios, NFw son visibles de adelante para atras, y NBw son visibles de atras para adelante, F es la lista de cantidad de pisos por edificio
    length(RLst,N),
    numlist(1,L,Numlist), 
    comb_perm(N, L,Numlist,RLst), % Se toma combinaciones de N tomados de L elementos, y se crean permutaciones de estas combinaciones
    numForw(L,RLst, NFw), %Ej: 2 tomados de 3: Combinaciones: [1,2], [1,3], [2,3] y se crean permutaciones de estos subconjuntos
    numBack(L,RLst, NBw).

comb(0,_,[]).

comb(N,[X|T],[X|Comb]) :-
     N>0,
     N1 is N-1,
     comb(N1,T,Comb).

comb(N,[_|T],Comb) :-
    N>0,
    comb(N,T,Comb).

comb_perm(N, L, List, Result) :-
    N < L,
    comb(N,List,Comb),
    permutation(Comb,Result).

comb_perm(N, N,List,Result) :-
    permutation(List,Result).


limPistaRelleno(N, NFill, Min, Max):-
    Min is max(3-NFill,1), 
    Max is N+1-NFill.

completarPista(N, N1, N2):-
    limPistaRelleno(N,N1,Min, Max),
    between(Min, Max, N2).

rellenarLista(N, Lista, ListaOrd):-
    NFin is N+1,
    rellenarListaR(NFin,1,Lista, ListaOrd).

rellenarListaR(N, I, [(I,H)|T], [(I,H)|JoinLst]):- %No se salto un indice
    INext is I + 1,
    rellenarListaR(N,INext,T,JoinLst).

rellenarListaR(N, Ind, [(I,H)|T], [(Ind, 0)|JoinLst]):- %Se salto un indce
    I > Ind,
    INext is Ind +1,
    rellenarListaR(N,INext,[(I,H)|T],JoinLst).

rellenarListaR(N, N, [], []).

rellenarListaR(N, Ind, [], [(Ind,0)|JoinLst]):-
    Ind < N,
    INext is Ind+1,
    rellenarListaR(N, INext, [], JoinLst).

rellenarListaR(N,Ind,[(I,_)|T], JoinLst):- %Eliminar repetidos
    Ind > I,
    rellenarListaR(N,Ind,T,JoinLst).

juntarPistas(1,_,_,[(1,1)]). %De solo haber un edificio, la unica solucion es [1] y las pistas tendran forma de (1,1)

juntarPistas(_,[],[],[]).

juntarPistas(N, [(I,NFw)|TFw], [(I,0)|TBw], [(NFw,NBw)|JoinLst]):-
    completarPista(N,NFw,NBw),
    juntarPistas(N,TFw, TBw, JoinLst).

juntarPistas(N, [(I,0)|TFw], [(I,NBw)|TBw], [(NFw,NBw)|JoinLst]):-
    completarPista(N,NBw,NFw),
    juntarPistas(N,TFw, TBw, JoinLst).

juntarPistas(N, [(I,0)|TFw], [(I,0)|TBw], [(NFw,NBw)|JoinLst]):-
    between(1,N,NFw),
    completarPista(N,NFw,NBw),
    juntarPistas(N,TFw, TBw, JoinLst).

juntarPistas(N, [(I,NFw)|TFw], [(I,NBw)|TBw], [(NFw,NBw)|JoinLst]):-
    NFw =\= 0,
    NBw =\= 0,
    Sum is NFw + NBw, 
    Max is N+1,
    between(3,Max,Sum),
    juntarPistas(N,TFw, TBw, JoinLst).
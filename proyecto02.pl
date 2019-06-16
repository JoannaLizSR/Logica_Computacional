% LABERINTO
%
% El laberinto a resolver por este programa es el siguiente :
%
%       0      1     2       3      4      5
%    _________________________________________
%   |      |      |      |      |      |      |
% 0 |   X  |      |      |  X   |      |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%   |      |      |      |      |      |      |
% 1 |   X  |      |   X  |  X   |   X  |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%   |      |      |      |      |      |      |
% 2 |      |      |      |  X   |      |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%   |      |      |      |      |      |      |
% 3 |   X  |  X   |      |      |      |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%   |      |      |      |      |      |      |
% 4 |      |      |   X  |   X  |  X   |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%   |      |      |      |      |      |      |
% 5 |      |  X   |  X   |      |      |      |
%   |      |      |      |      |      |      |
%   |______|______|______|______|______|______|
%
%
% Las filas y columnas del tablero se cuentan a partir
% de cero hasta cinco. Además las casillas marcadas con 
% una X son los muros del laberinto, es decir, estas casillas
% son inválidas para formar el camino entre las dos
% casillas elegidas.
%
%
%
%
% El conjunto de conocimiento son las casillas del laberinto.
% Solamente se declaran las casillas válidas, las cuales son
% las casillas que se pueden tomar para formar el camino o 
% caminos entre las dos casillas elegidas como inicio y fin.
%
%
% casilla(X,Y).
% X es la la fila y Y es la columna en la que está posicionada
% la casilla.
% X puede tomar los siguientes valores = {0,1,2,3,4,5}
% Y puede tomar los siguientes valores = {0,1,2,3,4,5}

casilla(0,1).
casilla(0,2).
casilla(0,4).
casilla(0,5).
casilla(1,1).
casilla(1,5).
casilla(2,0).
casilla(2,1).
casilla(2,2).
casilla(2,4).
casilla(2,5).
casilla(3,2).
casilla(3,3).
casilla(3,4).
casilla(3,5).
casilla(4,0).
casilla(4,1).
casilla(4,5).
casilla(5,0).
casilla(5,3).
casilla(5,4).
casilla(5,5).

% El tablero del laberinto se representa como las conexiones
% entre las casillas, es decir casillas vecinas. Por ejemplo, 
% la casilla (0,1) y (0,2) son casillas conectadas y además ambas
% son válidas. 
%
%
% conexion(casilla(a), casilla(b)).
% la casilla a y la casilla b estan conectadas en el tablero
% del laberinto.
% 
% También se representa la simetría de cada una de las casillas. 
% Si en el tablero está la conexion(casilla(a), casilla(b))
% entonces también está la conexion(casilla(b), casilla(a))
%
conexion(casilla(0,1), casilla(0,2)).
conexion(casilla(0,1), casilla(1,1)).
conexion(casilla(0,4), casilla(0,5)).
conexion(casilla(0,5), casilla(1,5)).
conexion(casilla(1,1), casilla(2,1)).
conexion(casilla(1,5), casilla(2,5)).
conexion(casilla(2,0), casilla(2,1)).
conexion(casilla(2,1), casilla(2,2)).
conexion(casilla(2,2), casilla(3,2)).
conexion(casilla(2,4), casilla(2,5)).
conexion(casilla(2,4), casilla(3,4)).
conexion(casilla(2,5), casilla(3,5)).
conexion(casilla(3,2), casilla(3,3)).
conexion(casilla(3,3), casilla(3,4)).
conexion(casilla(3,4), casilla(3,5)).
conexion(casilla(3,5), casilla(4,5)).
conexion(casilla(4,0), casilla(4,1)).
conexion(casilla(4,0), casilla(5,0)).
conexion(casilla(4,5), casilla(5,5)).
conexion(casilla(5,3), casilla(5,4)).
conexion(casilla(5,4), casilla(5,5)).

conexion(casilla(0,2), casilla(0,1)).
conexion(casilla(1,1), casilla(0,1)).
conexion(casilla(0,5), casilla(0,4)).
conexion(casilla(1,5), casilla(0,5)).
conexion(casilla(2,1), casilla(1,1)).
conexion(casilla(2,5), casilla(1.5)).
conexion(casilla(2,1), casilla(2,0)).
conexion(casilla(2,2), casilla(2,1)).
conexion(casilla(3,2), casilla(2,2)).
conexion(casilla(2,5), casilla(2,4)).
conexion(casilla(3,4), casilla(2,4)).
conexion(casilla(3,5), casilla(2,5)).
conexion(casilla(3,3), casilla(3,2)).
conexion(casilla(3,4), casilla(3,3)).
conexion(casilla(3,5), casilla(3,4)).
conexion(casilla(4,5), casilla(3,5)).
conexion(casilla(4,1), casilla(4,0)).
conexion(casilla(5,0), casilla(4,0)).
conexion(casilla(5,5), casilla(4,5)).
conexion(casilla(5,4), casilla(5,3)).
conexion(casilla(5,5), casilla(5,4)).




% Regla busca : 
% La regla se satisface si L es la lista con las casillas que 
% representará el camino entre las dos casillas elegidas.
%
% Descripción :
% Recibe como parámetro la casilla de inicio, la coordenada X1 
% es la fila y la coordenada Y1 es la columna de la casilla. 
% El segundo parámetro es la casilla de fin, la coordenada X2 
% es la fila y la coordenada Y2 es la columna de la casilla. 
%
% La regla regresará el camino o caminos existentes entre las
% dos casillas. El camino será representado como la lista L de 
% casillas, la cual iniciará con la casilla de inicio y al final
% la casilla fin. Las demás casillas siempre cumplirán con la
% condición de ser vecinas de la casilla anterior.
% 
% Ejemplos de entrada :
% ?- busca(casilla(0,1), casilla(5,3), Z).
% Z = [casilla(0, 1), casilla(1, 1), casilla(2, 1), casilla(2, 2), 
%      casilla(3, 2), casilla(3, 3), casilla(3, 4), casilla(3, 5), 
%      casilla(..., ...)|...]
%
% ?- busca(casilla(2,0), casilla(0,2), Z).
% Z = [casilla(2, 0), casilla(2, 1), casilla(1, 1), casilla(0, 1),
%      casilla(0, 2)] 
%
% ?- busca(casilla(5,5),casilla(2,5),Z).
% Z = [casilla(5, 5), casilla(4, 5), casilla(3, 5), casilla(2, 5)];
% Z = [casilla(5, 5), casilla(4, 5), casilla(3, 5), casilla(3, 4), 
%      casilla(2, 4), casilla(2, 5)] ;
% false.
% 
% ?- busca(casilla(5,0),casilla(1,5),Z).
% false.
% 
% ?- busca(casilla(5,3),casilla(4,1),Z).
% false. 


busca(casilla(X1,Y1), casilla(X2,Y2), L) :- 
                                    recorre_camino(X1,Y1,X2,Y2,L).




% Regla recorre_camino:
% La regla se satisface cuando C sea el camino entre las dos 
% casillas elegidas.
% 
% Descripción :
% Recibe como parámetro cuatro coordenadas : Xinicio, Yinicio, Xfin, 
% Yfin. 
% Las coordenadas Xini y Yini corresponden a las coordenadas de la
% casilla inicial y las coordenadas Xfin y Yfin corresponden a las
% coordenadas de la casilla final del camino C.
% 
% La casilla(Xini, Yini) siempre será la primer casilla en ser 
% visitada, por lo tanto siempre será la cabeza de lista.
% 
% Ejemplos de entrada :
% ?- recorre_camino(0,1,5,3,C).
% C = [casilla(0, 1), casilla(1, 1), casilla(2, 1), casilla(2, 2), casilla(3, 2), casilla(3, 3), casilla(3, 4), casilla(3, 5), casilla(..., ...)|...] ;
% C = [casilla(0, 1), casilla(1, 1), casilla(2, 1), casilla(2, 2), casilla(3, 2), casilla(3, 3), casilla(3, 4), casilla(2, 4), casilla(..., ...)|...] ;
% false.
%
% ?- recorre_camino(2,1,0,2,C).
% C = [casilla(2, 1), casilla(1, 1), casilla(0, 1), casilla(0, 2)] ;
%
% ?- recorre_camino(3,2,2,5,C).
% C = [casilla(3, 2), casilla(3, 3), casilla(3, 4), casilla(3, 5), casilla(2, 5)] ;
% C = [casilla(3, 2), casilla(3, 3), casilla(3, 4), casilla(2, 4), casilla(2, 5)] ;
% false.
%
% ?- recorre_camino(4,1,3,3,C).
% false.


recorre_camino(Xini, Yini, Xfin, Yfin, C) :-
 backtracking(Xini, Yini, Xfin, Yfin, [casilla(Xini, Yini)], C).


% Regla backtracking :
% La regla se sase satisface cuando C sea el camino entre las dos 
% casillas elegidas.
%
% Descripción : 
% La regla funciona bajo la idea de backtracking.
% En su forma básica, la idea de backtracking se asemeja a un 
% recorrido en profundidad dentro de un grafo dirigido. 
% El objetivo del recorrido es encontrar soluciones para algún 
% problema. Esto se consigue construyendo soluciones parciales a 
% medida que progresa el recorrido; estas soluciones parciales 
% limitan las regiones en las que se puede encontrar una solución 
% completa. El recorrido tiene éxito si, procediendo de esta forma, 
% se puede definir por completo una solución. En este caso el 
% algoritmo puede bien detenerse (si lo único que se necesita
% es una solución del problema) o bien seguir buscando soluciones 
% alternativas (si deseamos examinarlas todas).
% Por otra parte, el recorrido no tiene éxito si en alguna etapa la 
% solución parcial construida hasta el momento no se puede completar. 
% En tal caso, el recorrido vuelve atrás exactamente igual que en un 
% recorrido en profundidad, eliminando sobre la marcha los elementos 
% que se hubieran añadido en cada fase. Cuando vuelve a un nodo que 
% tiene uno o más vecinos sin explorar, prosigue el recorrido de una 
% solución.
%
% (a). Si las coordenadas de la casilla inicial y final son las 
%      mismas, entonces se trata de la misma casilla, por lo tanto
%      se regresa como camino a la lista con esa única casilla.
% (b). Se coloca como cabeza de la lista a la casilla(Xini,Yini), 
%      se busca a una casilla(Vx,Vy) la cual es la casilla vecina 
%      de la casilla inicial y como hasta ahora la casilla no había
%      sido visitada se continúa y se hace recursión ahora con la 
%      casilla(Vx,Vy) como cabeza y el resto del camino. En este punto 
%      lo que se quiere haceres buscar una nueva casilla vecina para 
%      ir formando el camino hasta llegar a la casilla final. Cada 
%      casilla se irá marcando como visitada para que la regla no vuelva 
%      a tomar esa casilla y explore nuevas opciones. Se termina cuando 
%      la última casilla visitada  sea la casilla final.


backtracking(Xini, Yini, Xini, Yini, _, [casilla(Xini, Yini)]).

backtracking(Xini, Yini, Xfin, Yfin, Visitada, 
	                            [casilla(Xini, Yini) | Camino]) :-
  conexion(casilla(Xini, Yini), casilla(Vx, Vy)),
  \+ pertenece(casilla(Vx, Vy), Visitada),
  backtracking(Vx, Vy, Xfin, Yfin, [casilla(Vx, Vy) | Visitada], 
  	                                                       Camino).

% Regla  pertenece :
% La regla se satisface si el elemento X está en la lista L.
%
% Descripción :
% (a). El caso base es comparar el elemento con la cabeza de la lista.
%      Si X es igual a la cabeza de la lista entonces la regla se
%      satisface.
% (b). La regla recursiva consta de ahora seguir con el siguiente elemento
%      de la lista y colocarlo como cabeza de la lista y si entra en el
%      caso base entonces la regla se satisface, de lo contrario se sigue
%      hasta encotrar el elemento.
%
% Ejemplos de entrada :
% ?- pertenece((0,1), [(2,0),(0,1),(0,5)]).
% true ;
%

pertenece(X, [X|_]).       

pertenece(X, [_|Elementos]) :- pertenece(X, Elementos).     
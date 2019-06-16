%Regla getnth : 
%              Recibe tres parámetros: E, N, L.
%              La regla será satisfactoria cuando el elemento E esté en 
%              el índice N de la lista L.

getnth(E,0,[E|_]).
getnth(E,N,[_|XS]) :- getnth(E,N1,XS), N is N1 + 1.



%Regla getIndex :
%                 Recibe tres parámetros: E, L, N.
%                 La regla será satisfactoria cuando el elemento E esté en 
%                 el índice N de la lista L.

getIndex(E,[E|_],0).
getIndex(E,[_|XS],N) :- getIndex(E,XS,P), N is P + 1.



%Regla num_ocurr :
%                  Recibe tres parámetros : E, L, R.
%                  La regla será satisfactoria cuando el elemento R ocurra
%                  R veces en la lista L.                   

num_ocurr(E,[],0).
num_ocurr(E,[X|XS],R) :- E==X, num_ocurr(E,XS,P), R is P + 1.
num_ocurr(E,[X|XS],R) :- E\=X, num_ocurr(E,XS,R).



%Regla concatena : 
%                  Recibe tres parámetros : L1, L2, LF.
%                  La regla será satisfactoria cuando L1 se concatene con L2,
%                  obteniendo la lista LF.

concatena([],[],[]).
concatena(X,[],X).
concatena([],Y,Y).
concatena([X|XS],YS,[X|ZS]) :- concatena(XS,YS,ZS).



%Regla deleteL :
%                Recibe tres parámetros : I, L , R.
%                La regla será satisfactoria cuando se elimine al elemento en
%                la I-ésima posición de la lista L. La lista resultante será
%                guardada en la variable R.

deleteL(0,[_|XS],XS).
deleteL(I,[X|XS],[X|ZS]) :- I > 0, I1 is I-1, deleteL(I1,XS,ZS).



%Regla sumaLista :
%                  Recibe dos parámetros : L, R.
%                  La regla será satisfactoria cuando R tenga el resultado de
%                  sumar los elementos de la lista L.

sumaLista([],0).
sumaLista([X|XS],R) :- sumaLista(XS,ZS), R is ZS + X.



%Regla acumulador : 
%                   Recibe dos parámetros : L, K.
%                   La regla será satisfactoria cuando K tenga el resultado de
%                   sumar los elementos anteriores al i-ésimo índice.
%                   K es una lista.

acumulador(L,K) :- acumulador_aux(L,0,K).



%Regla update :
%               Recibe cuatro parámetros : N , E , XS, R.
%               La regla actualiza el índice N de la lista XS con el elemento E.
%               El resultado se guarda en la variable R.

update(_,_,[],[]).
update(0,E,[_|XS],[E|XS]).
update(N,E,[X|XS],[X|YS]) :- N > 0, N1 is N-1, update(N1,E,XS,YS), !.



%Regla subset :
%               Recibe dos parámetros : L, K.
%               La regla será satisfactoria cuando K sea subconjunto de L.

subset([],[]).
subset([_|YS],K) :- subset(YS,K).
subset([X|YS],[X|XS]) :- subset(YS,XS).



%Regla union :
%              Recibe tres parámetros : A, B, C.
%              La regla será satisfactoria cuando C sea el resultado de unir
%              A con B.
%              En otras palabras, se debe cumplir C = A + B.

union([],[],[]).
union(A,[],A).
union([],B,B).
union([X|XS],YS,[X|ZS]) :- union(XS,YS,ZS).



%Regla splitL :
%               Recibe cuatro parámetros : L, N, L1, L2.
%               La regla será satisfactoria cuando se cumpla que la lista L
%               fue dividida en dos listas (L1 y L2), partiéndola en el índice N.

splitL([],_,[],[]).
splitL([X|XS],X,[X],XS).
splitL([Y|XS],X,[],[Y|XS]) :- X < Y.
splitL([Y|XS],X,[Y|LS],RS) :- X > Y, splitL(XS,X,LS,RS).



%Regla drop :
%             Recibe tres parámetros : N, L, R.
%             La regla será satisfactoria cuando se cumpla la propiedad de que se
%             eliminó cada N-ésimo elemento de la lista L.
%             La lista resultante se guarda en la variable R.

drop(N,L,R):- drop_aux(L,N,1,R).



%%%%%%%%%%%%%%%%%%%%%%%%% Funciones auxiliares %%%%%%%%%%%%%%%%%%%%%%%%%

%Regla acumulador_aux : 
%                       Recibe tres parámetros : L, S, K
%                       Acumula la suma de los elementos anteriores a la i-ésima
%                       posición.

acumulador_aux([],S,[]).
acumulador_aux([X|XS],S,[Y|YS]) :- Y is X + S, acumulador_aux(XS,Y,YS).



%Regla drop_aux :
%                 Recibe cuatro parámetros : L, N, M, R.
%                 Ignora el elemento en la posición N para que ya no aparezca en la
%                 lista resultante.

drop_aux([], _, _, []).
drop_aux([_|XS], N, N, R) :- drop_aux(XS, N, 1, R).
drop_aux([X|XS], N, M, [X|R]) :- M < N, M1 is M + 1, drop_aux(XS, N, M1, R).



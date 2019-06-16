persona(juan).

capital(usa, washington).
capital(uk, london).
capital(colombia, bogota).
capital(japon, tokio).
capital(usa, ny).
capital(mexico, ciudaddemexico).
capital(cabo_verde, praia).

paises(X,Y) :- persona(X), ingles(Y).
viajar(X,Z) :- paises(X,Y), capital(Y,Z).

moneda(uk, euros).
moneda(colombia, pesos).
moneda(mexico, pesos).
moneda(usa, dolar).
moneda(japon, yen).

lengua(usa, ingles).
lengua(uk, ingles).
lengua(mexico, espagnol).
lengua(colombia, espagnol).
lengua(japon, japones).




%Regla quevisitar : Recibe dos parámetros : C, L.
%C devolverá las capitales que podemos visitar hablando la lengua L.

quevisitar(C,L) :- capital(X,C) , lengua(X,L) .




%Regla quevisitar2 : Recibe tres parámetros: C, L, M.
%C devolverá las capitales que podemos visitar hablando la lengua L
%y pagando con la moneda M.

quevisitar2(C,L,M) :- capital(X,C) , lengua(X,L) , moneda(X, M) .

%-------------------------
%     Árbol genalógico
%         SIMPSON
%-------------------------
papa(homero, maggie).
papa(homero, lisa).
papa(homero, bart).
papa(abraham, homero).
papa(abraham, herbert).
papa(abraham, abbie).
papa(clancy, marge).
papa(clancy, selma).
papa(clancy, patty).
%papa(roberto, ling).
mama(marge, maggie).
mama(marge, lisa).
mama(marge, bart).
mama(mona, homero).
mama(edwina, abbie).
mama(???,herbert).
mama(jacqueline, marge).
mama(jacqueline, selma).
mama(jacqueline, patty).
mama(selma, ling).

%-------------------------


%Regla hijo: Recibe dos parámetros : Y,X.
%% Claramente Y es hijo de X si, X es papa de Y ó si X es mamá de Y.

hijo(Y,X) :- papa(X,Y); mama(X,Y). 




%Regla padres: Recibe tres parámetros: A, B, C.
%La regla será satisfactoria cuando A sea papá de C & B sea mamá de C.
%En otras palabras: A & B serán los papás de C.

padres(A,B,C) :- papa(A,C), mama(B,C) .




%Regla hermano: Recibe dos parámetros: M, N.
%La regla será satisfactoria cuando se cumpla la propiedad de que M es hermano de N.

hermano(M,N) :- padres(A,B,M) , padres(A,B,N) .




%Regla mediohermano: Recibe dos parámetros: D, E.
%La regla será satisfactoria cuando se cumpla la propiedad de que D es mediohermano de E.

mediohermano(D,E) :- (papa(Z,D), papa(Z,E), (mama(W,E), \+ mama(W,D))) ; (mama(Z,D), mama(Z,E), (papa(W,E), \+ papa(W,D)))  .




%Regla hss(mejor que hermanos_si_o_si): Recibe dos parámetros: A, B.
%La regla será satisfactoria cuando se cumpla la propiedad de que A es hermano o mediohermano de B.

hss(A,B) :- (padres(X,Y,A) , padres(X,Y,B)) ; ((papa(Z,A), papa(Z,B), (mama(W,B), \+ mama(W,A))) ; (mama(Z,A), mama(Z,B), (papa(W,B), \+ papa(W,A))) ) .




%Regla tio: Recibe dos parámetros: S, T.
%La regla será satisfactoria cuando se cumpla la propiedad de que S es tío de T.

tio(S,T) :- (hermano(S,X) ; mediohermano(S,X)) , (papa(X,T) ; mama(X,T)) .




%Regla primo: Recibe dos parámetros: U, V.
%La regla será satisfactoria cuando se cumpla la propiedad de que U es primo de V.

primo(U,V) :- padres(X,Y,U), padres(Z,W,V), (hermano(X,Z); hermano(X,W); hermano(Y,Z); hermano(Y,W)).



%Regla tutor: Recibe dos parámetros : A, B.
%La regla será satisfactoria cuando se cumpla la propiedad de que A es tutor de B.

tutor(A,B) :- papa(A,B) ; mama(A,B) . 




%Regla abuelo: Recibe dos parámetros : C, D.
%La regla será satisfactoria cuando se cumpla la propiedad de que C es abuelo de D.

abuelo(C,D) :- (papa(C,X) , papa(X,D)) ; (mama(C,X) , papa(X,D)) ; (papa(C,Y) , mama(Y,D)) ; (mama(C,Y) , mama(Y,D)) .




%Regla nieto : Recibe dos parámetros : E, F.
%La regla será satisfactoria cuando se cumpla la propiedad de que E es nieto de F.
nieto(E,F) :- hijo(E,X) , hijo(X,F) .




%Regla p : Recibe un parámetro : una lista
%La regla imprimirá la lista con dos flechitas entre cada elemento.

p([A|B]) :- print('→←') , print(A) , p(B) .
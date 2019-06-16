path(L, NI, NF, M) :- auxpath(L, L, NI, NF, M), !.

auxpath(L, [(A,B)|_], NI, NF, [C,D|_]) :- A == NI, B == NF, NI == C, NF == D, L \== [], !.
auxpath(L, [(A,B)|_], NI, NF, [C,D|_]) :- A == NF, B == NI, NI == C, NF == D, L \== [], !.
auxpath(L, [(A,B)|XS], NI, NF, [C|YS]) :- A == NI, NI == C -> auxpath(L, XS, B, NF, YS), !.
auxpath(L, [(A,B)|XS], NI, NF, [C|YS]) :- B == NI, NI == C -> auxpath(L, XS, A, NF, YS), !.
auxpath(L, [_|XS], NI, NF, [C|YS]) :- auxpath(L, XS, NI, NF, [C|YS]), !.
auxpath([_|XS], G, NI, NF, YS) :- G == [], YS \== [] -> auxpath(XS, XS, NI, NF, YS), !.
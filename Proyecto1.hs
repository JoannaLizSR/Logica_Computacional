module Proyecto1 where


{- INTEGRANTES:
   •López Pérez Frida Fernanda. •No. de cuenta: 315110520
   •Sánchez Rangel Joanna Lizeth. •No. de cuenta: 315060982
-}
    -- Definición del conjunto ATOM.
    data ATOM = Cte Bool
              | Var String

    -- Definición del lenguaje PROP.
    data PROP = FA ATOM
              | Neg  PROP
              | Conj PROP PROP
              | Disy PROP PROP
              | Impl PROP PROP
              | Syss PROP PROP
              deriving (Eq)

    -- Lista de estados.
    -- Verificar que sólo se usen variables.
    type Estado = ATOM  

    -- Instancia de ATOM con la clase Show.
    instance Show ATOM where
        show (Cte b) = show b
        show (Var v) = v
        
    -- Instancia de ATOM con la clase Eq
    instance Eq ATOM where
        ( Var p ) == ( Var q ) = p==q
        ( Cte p ) == ( Cte q ) = p==q

    -- Instancia de PROP con la clase Show.
    instance Show PROP where
        show (FA a)     = show a
        show (Neg p)    = "no " ++ show p
        show (Conj p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
        show (Disy p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
        show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
        show (Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"


    {-  Función vars: Recibe una proposición p y regresa una lista de variables atómicas.
            Obtiene la lista de variables que figuran en una fórmula proposicional.    
    -}    
    vars :: PROP -> [ATOM]
    vars p = eliminaRepetidos (varsAux p)


    {-  Función busca: Recibe una fórmula atómica y una lista de fórmulas atómicas.
            La función busca el una variable en la lista y si la encuentra regresa True, en
            caso contrario regresa False.
    -}
    busca :: ATOM -> [ATOM] -> Bool
    busca _ [] = False
    busca (Var v) ((Var x):xs) = v == x || busca (Var v) xs

    {-  Función interp : Recibe un proposición, una lista de estados y regresa un booleano.

            Dado un estado, la función interp realiza la interpretación de fórmulas 
            proposicionales.
        
            La interpretación de una fórmula atómica, donde ésta es un valor Booleano será:
                - Si el booleano es True, la interpretación será True sin importar que le 
                  pases como estado
                - Si el booleano es False, la interpretación será False sin importar que le 
                  pases como estado.
          
            La interpretación de una una fórmula atómica, donde ésta es es una Variable v, con 
            el estado xs. Se busca la variable v en el estado.

            La interpretacion de la negación de una proposición será la interpretación de p bajo 
            el estado. Obteniendo la interpretación de p, se buscará la interpretación contraria 
            con la función not del preludio de Haskell, ya que inicialmente se busca la negación.

            La interpretación de la conjunción de dos proposiciones p y q, sérá verdad si el 
            valor de p es verdad y el valor de q es verdad, es decir p y q deberán ser ambas 
            forzosamente verdaderas.Para verificar esto se utiliza el operador && de Haskell.
            Después se hace recursión sobre las proposiciones p y q.

            La interpretación de la disyunción de dos proposiciones p y q, sérá verdad si el valor 
            de p es verdad o el valor de q es verdad. Es decir, sólo nos importa que alguna de las 
            dos sea verdadera. Para verificar esto se utiliza el operador || de Haskell.
            Después se hace recursión sobre las proposiciones p y q.

            La interpretación de la implicación se hará bajo la equivalencia lógica:
                    p -> q   = ¬ p ∨ q
            Por lo tanto, se hace recursión sobre la interpretacion de no p (usando la función not 
            de Haskell) y la recursión de la interpretación de q.

            La interpretación de la equivalencia es verdad si la interpretación de p es igual a la 
            interpretación de q.
    -}
    interp :: PROP -> [Estado] -> Bool
    interp (FA (Cte c)) _ = c
    interp (FA (Var v)) xs = busca (Var v) xs
    interp (Neg p) xs = not (interp p xs)
    interp (Conj p q) xs = (interp p xs) && (interp q xs)
    interp (Disy p q) xs = (interp p xs) || (interp q xs)
    interp (Impl p q) xs = not (interp p xs) || (interp q xs)
    interp (Syss p q) xs = (interp p xs) == (interp q xs)

    {-  Función subconjuntos:
            Recibe una lista y obtiene las sublistas de la lista pasada como parámetro.
    -}
    subconjuntos :: [a] -> [[a]]
    subconjuntos [] = [[]]
    subconjuntos (x:xs) = [x:ys | ys <- xss] ++ xss
        where xss = subconjuntos xs

    {-  Función estados: Recibe una Proposición p y regresa una lista de listas de estados.
            Obtiene los posibles estados de las variables de una fórmula proposicional.
    -}
    estados :: PROP -> [[Estado]]
    estados p = subconjuntos (vars p)

    {-  Función esTautología : Recibe una proposición y regresa True si la proposición es  una
                               Tautología, en caso contrario regresa False.
            
            Por definición, la fórmula f es tautología si se evaluan verdadero en todos los
            estados posibles.
    -}
    esTautologia :: PROP -> Bool
    esTautologia f = null (filter (\y -> (y == False)) (map (\x -> interp f x) (estados f)))

    {-  Función es Satisfacible : Recibe una proposición y regresa True si la proposición f es
                                  satisfacible, en caso contrario regresa False.
            Por definición, la proposición f es satisfacible si existe un modelo que haga 
            verdadera a f.
    -}
    esSatisfacible :: PROP -> Bool
    esSatisfacible f = not (esContradiccion f)

    {-  Función esContradicción: Recibe una proposición y regresa True si la proposición f es
                                 contradicción, en caso contrario regresa True.
            Por definición, la proposición f es contradicción si se evalúan a falso en todos los
            estados.
    -}
    esContradiccion :: PROP -> Bool            
    esContradiccion f = null (filter (\y -> (y == True)) (map (\x -> interp f x) (estados f)))
            
    {-  Función eliminaRepetidos: Recibe una lista y regresa la lista sin elementos repetidos.
        Es decir, deja la primera aparición del elemento que encuentra.
    -}
    eliminaRepetidos :: Eq a => [a] -> [a]
    eliminaRepetidos [] = []
    eliminaRepetidos (x:xs) = x:(eliminaRepetidos (filter (\y->y/=x) xs))


    {-  Función iN : Recibe una proposición y regresa una proposición donde introduce la negación.
    -}
    iN :: PROP -> PROP
    iN (FA a) = FA a
    iN (Neg p) = iNA p
    iN (Conj p q) = Conj (iN p) (iN q)
    iN (Disy p q) = Disy (iN p) (iN q)
    iN p = p

    {-  Función fnn : Recibe una proposición y regresa la forma normal negativa de la proposición.
            Por definición, la forma normaal negativa de una proposición es proposición que no tenga
            implicaciones y equivalencias. Además que la negación sólo afecta a variables atómicas.
    -}
    fnn :: PROP -> PROP
    fnn p = iN (eI (eE p))

    {-  Función eE : Recibe una proposición y regresa una proposición sin equivalencias.
                p <-> q = (p -> q) ∧ (q -> p) 
    -}
    eE :: PROP -> PROP
    eE (FA a) = (FA a)
    eE (Neg p) = Neg(eE p)
    eE (Conj p q) = Conj (eE p) (eE q)
    eE (Disy p q) = Disy (eE p) (eE q)
    eE (Impl p q) = Impl (eE p) (eE q)
    eE (Syss p q) = Conj (Impl (eE p) (eE q)) (Impl (eE q) (eE p))

    {-  Función eI : Recibe una proposición y regresa una proposición sin implicaciones.
                p -> q = ¬ p ∨ q
        
    -}     
    eI :: PROP -> PROP
    eI (FA a) = (FA a)
    eI (Neg p) = Neg (eI p)
    eI (Conj p q) = Conj (eI p) (eI q)
    eI (Disy p q) = Disy (eI p) (eI q)
    eI (Impl p q) = Disy (Neg (eI p)) (eI q)
    eI (Syss p q) = Conj (eI (Impl p q)) (eI (Impl q p))

    {-  Función fnc: Recibe una proposición y regresa la forma Normal conjuntiva de la 
                     proposición.
            Por definición, una proposición está en forma normal conjuntiva si y sólo si
            está de la forma:
                                C1 ∧ C2 ∧ C3 ∧ ... ∧ Ci    donde 1 <= i , y Ci es una cláusula.
        o
    -}
    fnc :: PROP -> PROP
    fnc p = fnca (fnn p)

    {-  Función: dD : Recibe dos proposiciones y lo que hace es regresar uan proposición con las 
                      disyunciones distribuídas.
    -}
    dD :: PROP -> PROP -> PROP
    dD (Conj p q) r = Conj (dD q r) (dD p r)
    dD r (Conj p q) = Conj (dD r p) (dD r q)
    dD p q = Disy p q

    {-  Función alfaregla : Recibe una lista de list de Proposiciones y regresa una lista de proposiciones aplicando 
                            alfa regla.
            Se siguen las siguientes reglas :
                - De A ∧ B se dedce A y B
                - De ¬(A ∨ B) se deduce ¬A y ¬B
                - De ¬(A -> B) se deduce A y ¬B
    -}
    alfaregla :: [[PROP]] -> [[[PROP]]]
    alfaregla [] = []
    alfaregla [[]] = []
    alfaregla [(x:xs)] = [alfaAux (x:xs)]
    alfaregla ((x:xs):ys) = [alfaAux (x:xs)] ++ (alfaregla ys)

    {-  Función betaregla : Recibe una proposición y regresa una lista de proposiciones aplicando 
                            beta regla.
            Se siguen las siguientes reglas :
                - De A ∨ B se dedce A y, en una lista distinta B
                - De ¬(A ∧ B) se deduce ¬A, y en una lista distinta ¬B
                - De (A -> B) se deduce ¬A, y en una lista distinta ¬B
    -}
    betaregla :: [[PROP]] -> [[[PROP]]]
    betaregla [] = []
    betaregla [[]] = []
    betaregla [(x:xs)] = betaAux (x:xs)
    betaregla ((x:xs):ys) = betaAux (x:xs) ++ (betaregla ys)

    {-  Función tableau: Recibe una proposición y dibuja su Tableau
    -}
    tableau :: PROP -> [[[PROP]]]
    tableau (FA a) = [[[FA a]]]
    tableau a = betareglaAux (betareglaAux (betareglaAux [[[fnc a]]]))


    -----------------------------------------FUNCIONES AUXILIARES-----------------------------------------

    {-  Función varsAux: Recibe una proposición p y regresa una lista de variables atómicas.
            En general, la función obtiene la lista de variables que figuran en una fórmula 
            proposicional.
    -}
    varsAux :: PROP -> [ ATOM ]
    varsAux (FA (Cte b)) = []
    varsAux (FA p) = [p]
    varsAux (Neg x) = varsAux x
    varsAux (Conj x y) = (varsAux x) ++ (varsAux y)
    varsAux (Disy x y) = (varsAux x) ++ (varsAux y)
    varsAux (Impl x y) = (varsAux x) ++ (varsAux y)
    varsAux (Syss x y) = (varsAux x) ++ (varsAux y)

    {-  Función iNA: Recibe una proposición e introduce las negaciones a la proposición.
    -}
    iNA :: PROP -> PROP
    iNA (FA a) = Neg (FA a)
    iNA (Neg p) = iN p
    iNA (Conj p q) = Disy (iNA p) (iNA q)
    iNA (Disy p q) = Conj (iNA p) (iNA q)
    iNA p = p

    {-  Función fnca: Recibe una proposición y regresa su forma normal conjuntiva.
    -}
    fnca :: PROP -> PROP
    fnca (FA p) = (FA p)
    fnca (Disy p q) = dD p q
    fnca p = p

    {- Función alfaAux: Recibe una lista de proposiciones y aplica las reglas para 
                        alfa regla.
    -}
    alfaAux :: [PROP] -> [[PROP]]
    alfaAux [] = []
    alfaAux ((FA a):xs) = [[FA a]] ++ alfaAux xs
    alfaAux ((Neg a):xs) = [[Neg a]] ++ alfaAux xs
    alfaAux ((Conj a b):xs) = alfaAux [a] ++ alfaAux [b] ++ alfaAux xs
    alfaAux ((Disy a b):xs) = [[Disy a b]] ++ alfaAux xs

    {-Función betaAux: Recibe una lista de proposiciones y aplica las reglas para 
                       beta regla.
    -}
    betaAux :: [PROP] -> [[[PROP]]]
    betaAux [] = []
    betaAux (x:xs) = betaAux2 x ++ betaAux xs 

    betaAux2 :: PROP -> [[[PROP]]]
    betaAux2 (FA a) = [[[FA a]]]
    betaAux2 (Neg a) = [[[Neg a]]]
    betaAux2 (Conj a b) = [alfaAux [Conj a b]]
    betaAux2 (Disy a b) = betaAux2 a ++ betaAux2 b

    betareglaAux :: [[[PROP]]] -> [[[PROP]]]
    betareglaAux [] = []
    betareglaAux [[]] = []
    betareglaAux [[[]]] = []
    betareglaAux (x:xs) = betaregla x ++ betareglaAux xs

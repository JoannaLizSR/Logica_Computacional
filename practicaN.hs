module PracticaN where
import Data.List

-- Las variables proposicionales son del tipo Char.
type VarP = Char

{- Los estados son listas de tuplas donde la primer componente de la tupla es 
   una variable proposicional y su segundo componente será el valor booleano 
   asociado a dicha variable.
-}
type Estado = [(VarP, Bool)]

-- El tipo de dato para representar las fórmulas proposicionales.
data Prop = Top -- True
          | Bot -- False
          | Var VarP  -- Var "R"
          | Neg Prop   -- ~P
          | Conj Prop Prop -- P ^ Q
          | Disy Prop Prop -- P v Q
          | Impl Prop Prop -- P -> Q
          | Syss Prop Prop -- P <-> Q
          deriving (Eq,  Ord, Show)



{- Función correcto. Recibe una lista de fórmulas proposicionales (gamma) y una 
   conclusión (phi). Nos dice si el argumento es lógicamente correcto o no.

   Descripción:
   Un argumento con premisas A1, A2, ..., An y una conclusión B es lógicamente
   correcto si {A1, A2, ..., An} es consecuencia lógica de B. Así, basta saber
   si (gamma) es consecuencia lógica de (phi).

   Ejemplos de entrada:
   *PracticaN> correcto [Impl(Var 'P')(Var 'Q'), Impl(Var 'R')(Var 'S'), 
   Disy(Var 'P')(Var 'R'), Neg(Conj(Var 'Q')(Var 'S'))] 
   (Impl(Var 'P')(Conj(Neg(Var 'R'))(Var 'S')))
   False

   *PracticaN> correcto [Impl(Var 'P')(Var 'Q'), Impl(Var 'Q')(Var 'R'), 
   Disy(Conj(Var 'P')(Neg(Var 'R')))(Conj(Var 'R')(Neg(Var 'P')))] (Var 'R')
   True
-}
correcto :: [Prop] -> Prop -> Bool
correcto gamma phi = consecuencia gamma phi


data Graph = Graph { gId :: Int,
                     nodes :: [String],
                     edges :: [(String, String)]
                   }deriving (Show, Eq)



{- Función path. Recibe dos cadenas y la gráfica. Las cadenas representan los nodos
   de la gráfica. Nos regresa True si entre dos nodos existe un camino y False en 
   otro caso.

   Descripción :
   Si la lista de puentes de la gŕáfica es vacía, entoces en la gráfica no existirá
   ningún nodo que sea adyacente a otro, por lo tanto no habrá ningún camino y la 
   función regresará False.

   Si el nodo inicial pertenece a la lista de vecinos del nodo final, entonces existe
   un camino entre ellos.

   En otro caso, revisa los vecinos de los vecinos hasta encontrar si existe un camino
   entre ambis nodos.

   Ejemplos de entrada:
   *PracticaN> let g = Graph{gId = 1, nodes = ["A","B","C","D"], edges = [("A","B"),("C","D")]}
   
   *PracticaN> path "A" "C" g
   False
   
   *PracticaN> path "A" "B" g
   True

   *PracticaN> path "C" "D" g
   True

   *PracticaN> path "D" "C" g
   True

   *PracticaN> path "B" "A" g
   True
-}
path :: String -> String -> Graph -> Bool
path nodoInicial nodoFinal (Graph {gId = gId, nodes = n, edges = []}) = False
path nodoInicial nodoFinal grafica
                | (elemm nodoInicial (neighbors nodoFinal grafica)) = True
                | otherwise = (auxPath grafica nodoInicial (neighbors nodoFinal grafica))




-- Funciones auxiliares --

{- |Aux. 1 | Función buscaBool. Recibe una variable proposicional varP, y 
   un estado e = [(VarP, Bool)]. Regresa la segunda componente del primer par 
   ordenado de la lista de estados I, cuyo primer componente sea igual a la
   variable varP. Es decir, regresa el valor booleano asociado a la primer 
   variable proposicional varP que encuentre.

   Ejemplos de entrada:
   *Main> buscaBool 'r' [('p', True), ('q', True), ('r', False)]
   False

   *Main> buscaBool 'x' [('l', False), ('x', True), ('m', True), ('s', True)]
   True
-}
buscaBool :: (Eq varP) => varP -> [(varP, Bool)] -> Bool
buscaBool v e = head [b | (v',b) <- e, v == v']

{- |Aux. 2| Función interp. Recibe una fórmula (phi) y un estado e. Regresa la 
   interpretación de phi con el estado dado.

   Descripción:
   Siguiendo nuestra definición de interpretación, sabemos que
   - I(v) = v, donde v es una variable proposicional. 
   - I(Top) = 1
   - I(Bot) = 0
   - I(~P) = 1 <-> I(P) = 0
   - I(P ^ Q) = 1 <-> I(P) = I(Q) = 1
   - I(P v Q) = 0 <-> I(P) = I(Q) = 0
   - I(P -> Q) = 0 <-> I(P) = 1 e I(Q) = 0
   - I(P <-> Q) = 1 <-> I(P) = I(Q)
   
   donde la única modificación que hacemos en nuestra implementación es para 
   el operador Impl, ya que utilizamos una equivalencia lógica para facilitar
   el cálculo de la interpretación de dicho operador.

   Ejemplos de entrada:
   *Main> interp (Conj (Var 'p') (Var 'q')) [('p', True), ('q', False)]
   False

   *Main> interp (Disy (Neg (Var 'x')) (Neg (Var 'y'))) [('x', False), ('y', True)]
   True
-}
interp :: Prop -> Estado -> Bool
interp phi e = case phi of 
    Top -> True
    Bot -> False 
    Var v -> buscaBool v e
    Neg p -> not (interp p e)
    Conj p q -> (interp p e) && (interp q e)
    Disy p q -> (interp p e) || (interp q e)
    Impl p q -> not (interp p e) || (interp q e)
    Syss p q -> (interp p e) == (interp q e)

{- |Aux. 3| Función vars. Recibe una formula (phi). Regresa la lista de 
   variables proposicionales que figuran en (phi), sin repetición.

   Descripción: 
   Simplemente utilizamos la función 'union' para unir las listas finales
   que contienen las variables proposicionales de cada una de las fórmulas. 
   Por cómo está definida 'union', nos regresa la lista que contiene todos 
   los elementos de las listas que recibe, sin repetición.

   Ejemplos de entrada:
   *Main> vars (Syss (Impl (Var 'p') (Var 'r')) (Conj (Var 'q') (Var 's')))
   "prqs"

   *Main vars (Disy (Neg (Var 'x')) (Neg (Var 'y')))
   "xy"
-}
vars :: Prop -> [VarP]
vars phi = case phi of
    Top -> []
    Bot -> []
    Var i -> [i]
    Neg p -> vars p
    Conj p q -> vars p `union` vars q
    Disy p q -> vars p `union` vars q
    Impl p q -> vars p `union` vars q
    Syss p q -> vars p `union` vars q

{- |Aux. 4| Función varsConj. Recibe una lista de fórmulas (gamma). Regresa la
   lista de variables proposicionales que figuran en todas las fórmulas que 
   están en gamma.

   Descripción:
   Utilizamos una lista de comprensión para obtener todas las variables que 
   figuran en las fórmulas que contiene gamma. Utilizamos concat para obtener
   la lista de listas que tenemos después de aplicar la función "vars" a cada
   una de las fórmulas que tiene gamma.

   Ejemplos de entrada:
   *PracticaN> varsConj [Impl(Var 'P')(Var 'Q'), Disy(Var 'P')(Var 'R')]
   "PQPR"

   *PracticaN> varsConj [Conj(Neg(Var 'J'))(Var 'A'), Syss(Var 'D')(Var 'K'), 
   Disy(Var 'K')(Var 'P'), Impl(Var 'R')(Var 'M'), Neg(Var 'A')]
   "JADKKPRMA"
-}
varsConj :: [Prop] ->[VarP]
varsConj gamma = concat [vars psi | psi <- gamma]

{- |Aux. 5| Función estadosConj. Recibe una lista de fórmulas (gamma). Regresa
   la lista con los estados posibles para (phi).

   Descripción:
   Para obtener la lista de estados, debemos obtener el subconjunto de listas
   de la lista de variables proposicionales de (gamma), hacer las combinaciones
   posibles entre los estados y los valores booleanos e ir concatenando las
   tuplas para formar una lista (que es justo lo que hacemos en la función
   "subconj"), Y como subconj trabaja con listas de comprensión, al final 
   tendremos una lista de listas con los estados deseados.

   Ejemplos de entrada:
   *PracticaN> estadosConj [Disy(Neg(Var 'P'))(Var 'Q'), Impl(Var 'R')(Var 'S')]
   [[('P',True),('Q',True),('R',True),('S',True)], 
   [('P',True),('Q',True),('R',True),('S',False)],
   [('P',True),('Q',True),('R',False),('S',True)],
   [('P',True),('Q',True),('R',False),('S',False)],
   [('P',True),('Q',False),('R',True),('S',True)],
   [('P',True),('Q',False),('R',True),('S',False)],
   [('P',True),('Q',False),('R',False),('S',True)],
   [('P',True),('Q',False),('R',False),('S',False)],
   [('P',False),('Q',True),('R',True),('S',True)],
   [('P',False),('Q',True),('R',True),('S',False)],
   [('P',False),('Q',True),('R',False),('S',True)],
   [('P',False),('Q',True),('R',False),('S',False)],
   [('P',False),('Q',False),('R',True),('S',True)],
   [('P',False),('Q',False),('R',True),('S',False)],
   [('P',False),('Q',False),('R',False),('S',True)],
   [('P',False),('Q',False),('R',False),('S',False)]]
-}
estadosConj :: [Prop] -> [Estado]
estadosConj gamma = subconj (varsConj gamma)
    where subconj [] = [[]]
          subconj (x:xs) = 
            [(x,True):i | i <- subconj xs] ++ [(x,False):i | i <- subconj xs] 

{- |Aux. 6| Función satisfenConj. Recibe un estado e y una lista de fórmulas 
   (gamma). Nos dice si el conjunto de fórmulas es satisfacible con el estado
   dado.

   Descripción:
   Utilizamos una lista de comprensión para obtener la lista de listas con la 
   interpretación de cada una de las fórmulas con el estado e. Utilizamos 
   "and" para determinar si todas las interpretaciones obtenidas son verdaderas
   (en tal caso gamma es satisfacible) o si alguna de ellas es falsa (en tal 
   caso gamma no es satisfacible).

   Ejemplos de entrada:
   *PracticaN> satisfenConj [('S', False), ('P', True), ('Q', False)] 
   [Impl(Var 'P')(Var 'Q'), Conj(Disy(Var 'S')(Var 'P'))(Neg(Var 'Q')), 
   Neg(Var 'S')]
   False

   *PracticaN> satisfenConj [('P', False), ('R', False), ('S', False), 
   ('T', False), ('Q', True)] [Impl(Var 'P')(Neg(Var 'Q')), 
   Impl(Disy(Var 'R')(Var 'S'))(Var 'T'), Impl(Var 'T')(Var 'Q')]
   True
-}
satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj e phi = and [satisfen e psi | psi <- phi]

{- |Aux. 7| Función satisfen. Recibe un estado e y una fórmula (phi). Nos dice
   si (phi) es satisfacible con el estado dado.

   Descripción:
   La función es simple. Si la interpretación de (phi) con el estado dado es 
   verdadera, entonces (phi) es satisfacible por definición. 

   Ejemplos de entrada:
   *PracticaN> satisfen [('P', True), ('R', False), ('S', True)] 
   (Disy(Impl(Var 'P')(Var 'R'))(Conj(Neg(Var 'S'))(Var 'P')))
   True

   *PracticaN> satisfen [('P', True), ('R', False), ('S', True)] 
   (Disy(Impl(Var 'P')(Var 'R'))(Conj(Neg(Var 'S'))(Var 'P')))
   False
-}
satisfen :: Estado -> Prop -> Bool
satisfen i phi = interp phi i == True

{- |Aux. 8| Función consecuencia. Recibe una lista de fórmulas gamma y una 
   conclusión (phi). Nos dice si (phi) es consecuencia lógica de (gamma).

   Descripción:
   Para mostrar que (phi) es consecuencia lógica de (gamma) basta verificar que
   no existen modelos de (gamma) que no sean modelos de (phi), es decir, que la
   lista de modelos de (gamma) que no son modelos de (phi) es vacía. Esto lo 
   hacemos usando una lista de comprensión, donde escribimos las 
   espeficicaciones antes mencionadas.

   Ejemplos de entrada:
   *PracticaN> consecuencia [Impl(Var 'P')(Conj(Var 'Q')(Var 'R'))] 
   (Disy(Impl(Var 'P')(Var 'Q'))(Impl(Var 'P')(Var 'R')))
   True

   *PracticaN> consecuencia [Impl(Var 'P')(Var 'Q'), Impl(Var 'R')(Var 'S'), 
   Disy(Var 'P')(Var 'R'), Neg(Conj(Var 'Q')(Var 'S'))] 
   (Conj(Impl(Var 'Q')(Var 'P'))(Impl(Var 'S')(Var 'R')))
   True
-}
consecuencia :: [Prop] -> Prop -> Bool
consecuencia gamma phi = null [i | i <- estadosConj (phi: gamma), 
                               satisfenConj i gamma, not (satisfen i phi)]

{- |Aux. 9| Función auxPath. Recibe una gráfica, una cadena y una lista de cadenas.
   La cadena representa el nodo inicial y la lista de cadenas sus vecinos. 
   Nos regresa True si entre dos nodos existe un camino y False en otro caso.

   Descripción:
   Si la lista de vecinos es vacía, entonces no existen vestices adyacentes en la
   gráfica y por lo tanto no existe ningún camino, así que la función regresará 
   False como resultado.

   En otro caso, revisa que el nodo inicial pertenezca a la lista de vecinos o
   a los vecinos de sus vecinos.   
-}

auxPath :: Graph -> String -> [String] -> Bool
auxPath grafica nodoInicial [] = False
auxPath grafica nodoInicial (vecinos:xs) = (elemm nodoInicial (neighbors vecinos grafica)) || 
                                           (auxPath grafica nodoInicial xs)


{-  |Aux. 10| Función neighbors: Recibe un nodo, una gráfica y regresa la lista de nodos 
    que son vecinos del nodo dado como parámetro.
    
    Descripción:
    Si la lista de puentes es vacía, en la gráfica no existirá ningún vértice que sea
    adyacente, por lo tanto la lista de vecinos será vacía.

    En otro caso, se busca entre las tuplas al nodo hasta encontrarlo y se regresa la
    lista de nodos con los que estaba emparejado.

    Ejemplos de entrada:
    *PracticaN> let g = Graph{gId = 1, nodes = ["A","B","C","D"], 
    edges = [("A","B"),("C","D"),("D","A")]}
    
    *PracticaN> neighbors "A" g
    ["B","D"]
-}
neighbors :: String -> Graph -> [String]
neighbors s (Graph {gId = gId, nodes = n, edges = []}) = []
neighbors s (Graph {gId = gId, nodes = n, edges = ((x,y):xs)}) 
 | s == x = y:(neighbors s (Graph {gId = gId, nodes = n, edges = xs}))
 | s == y = x:(neighbors s (Graph {gId = gId, nodes = n, edges = xs}))
 | otherwise = (neighbors s (Graph {gId = gId, nodes = n, edges = xs}))

{- |Aux. 11| Función elemm : Recibe un elemento y una lista. Nos dice si el elemento
   pertenece a la lista pasada como parámetro.

   Descripción: Si la lista entonces nos regresará False como resultado. En otro caso,
   revisa si el elemento x es igual a la cabeza de la lista, de ser así nos regresa True,
   en otro caso se hace recursión sobre la cola de la lista y se repite lo anterior.

   Ejemplos de entrada:
   *PracticaN> elemm 'a' ['a','b']
   True
-}

elemm :: (Eq a) => a -> [a] -> Bool
elemm _ []       = False
elemm x (y:ys)   = x == y || elem x ys
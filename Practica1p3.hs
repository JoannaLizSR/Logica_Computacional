module Practica1p3 where


data Graph = Graph { gId :: Int, 
                   nodes :: [String],
                  edges :: [(String, String)]
                  } deriving (Show, Eq)


{-
Función neighbors: Recibe un nodo, una gráfica y regresa la lista de nodos 
que son vecinos del nodo dado como parámetro.
-}
neighbors :: String -> Graph -> [String]
neighbors s (Graph {gId = gId, nodes = n, edges = []}) = []
neighbors s (Graph {gId = gId, nodes = n, edges = ((x,y):xs)}) 
 | s == x = y:(neighbors s (Graph {gId = gId, nodes = n, edges = xs}))
 | s == y = x:(neighbors s (Graph {gId = gId, nodes = n, edges = xs}))
 | otherwise = (neighbors s (Graph {gId = gId, nodes = n, edges = xs}))


{-
Función mindegree: Recibe una gráfica y nos devuelve el grado menor 
de la gráfica.
-}
mindegree :: Graph -> Int
mindegree (Graph {gId = gId, nodes = [], edges = e}) = 0
mindegree (Graph {gId = gId, nodes = [x], edges = e}) = aux x e
mindegree (Graph {gId = gId, nodes = (x:xs), edges = e}) = 
  min (aux x e) (mindegree(Graph {gId = gId, nodes = xs, edges = e}))



{-
Función maxdegree: Recibe una gráfica y devuelve el grado mayor de la gráfica.
-}
maxdegree :: Graph -> Int 
maxdegree (Graph {gId = gId, nodes = [], edges = e}) = 0
maxdegree (Graph {gId = gId, nodes = [x], edges = e}) = aux x e
maxdegree (Graph {gId = gId, nodes = (x:xs), edges = e}) = 
  max (aux x e) (mindegree(Graph {gId = gId, nodes = xs, edges = e}))


{-
delete :: String -> Graph -> Graph
delete = error "Implementar"
-}

-- Tipo de dato para representar expresiones de la lógica proposicional
data Prop = TTrue
           | FFalse 
           | Var String
           | Neg Prop
           | Conj Prop Prop
           | Disy Prop Prop
           | Impl Prop Prop
           | Syss Prop Prop
           deriving (Eq,Ord,Show)

type Estado = [(String, Prop)]


{-
  Función simplify: Recibe una Prop (expresión de la lógica proposicional)
  y regresa otra Prop pero sin dobles negaciones.
-}
simplify :: Prop -> Prop
simplify TTrue = TTrue
simplify FFalse = FFalse
simplify (Var p) = (Var p)
simplify (Neg p) = simplifyaux p
simplify (Conj p q) = (Conj (simplify p) (simplify q))
simplify (Disy p q) = (Disy (simplify p) (simplify q))
simplify (Impl p q) = (Impl (simplify p) (simplify q))
simplify (Syss p q) = (Syss (simplify p) (simplify q))


{-
  Función deleteImpl: Recibe una Prop (expresión de la lógica proposicional)
  y regresa otra Prop que elimina implicaciones y equivalencias mediante 
  equivalencia de operadores.
-}
deleteImpl :: Prop -> Prop
deleteImpl TTrue = TTrue
deleteImpl FFalse = FFalse
deleteImpl (Var p) = (Var p)
deleteImpl (Neg p) = (Neg (deleteImpl p))
deleteImpl (Conj p q) = (Conj (deleteImpl p) (deleteImpl q))
deleteImpl (Disy p q) = (Disy (deleteImpl p) (deleteImpl q))
deleteImpl (Impl p q) = (Disy (Neg(deleteImpl p)) (deleteImpl q))
deleteImpl (Syss p q) = deleteImpl((Conj (Impl (deleteImpl p) (deleteImpl q)) (Impl(deleteImpl q) (deleteImpl p)) ))


{-
  Función demorgan: Recibe una Prop (expresión de la lógica proposicional)
  y regresa, mediante equivalencia de las reglas de De Morgan otra Prop 
  equivalente.
-}
demorgan :: Prop -> Prop
demorgan TTrue = TTrue
demorgan FFalse = FFalse
demorgan (Var p) = (Var p)
demorgan (Neg p) = demorgan p
demorgan (Conj p q) = (Disy (Neg(demorgan p)) (Neg(demorgan q)))
demorgan (Disy p q) = (Conj (Neg(demorgan p)) (Neg(demorgan q)))
demorgan (Impl p q) = (Impl (demorgan p) (demorgan q))
demorgan (Syss p q) = (Syss (demorgan p) (demorgan q))


{-
  Función inter: Recibe una Prop (expresión de la lógica proposicional),
  un estado y regresa un booleano, el cual es la interpretación de la
  proposición haciendo uso del estado dado.
-}
inter :: Prop -> Estado -> Bool
inter TTrue c = True
inter FFalse c = False
inter (Var p) c = True
inter (Neg p) c = (not (inter p c))
inter (Conj p q) c = (inter p c) && (inter q c) 
inter (Disy p q) c = (inter p c) || (inter q c)
inter (Impl p q) c = (not(inter p c)) || (inter q c)
inter (Syss p q) c = (inter p c) == (inter q c)


{-
  Función fnc: Recibe una Prop (expresión de la lógica proposicional)
  y devuelve la forma normal conjuntiva de la fórmula.
-}
fnc :: Prop -> Prop
fnc p = (distribuyeDisy (fnn p))


--esFNC :: Prop -> Bool
--esFNC = error "Implementar"

--correcto :: [Prop] -> Prop -> Bool
--correcto = error "Implementar"


--FUNCIONES AUXILIARES

{-
  Función simplifyaux: Recibe una Prop (expresión de la lógica proposicional)
  y nos regresa la Prop sin las doble negaciones en expresiones compuestas.
-}
simplifyaux :: Prop -> Prop
simplifyaux TTrue = TTrue
simplifyaux FFalse = FFalse
simplifyaux (Var p) = (Neg(Var p))
simplifyaux (Neg p) = simplify p
simplifyaux (Conj p q) = (Disy (simplifyaux p) (simplifyaux q))
simplifyaux (Disy p q) = (Conj (simplifyaux p) (simplifyaux q))
simplifyaux (Impl p q) = (Impl (simplifyaux p) (simplifyaux q))
simplifyaux (Syss p q) = (Syss (simplifyaux p) (simplifyaux q))


{-
  Función distribuyeDisy: Recibe una Prop (expresión de la lógica proposicional)
  y regresa esa Prop, pero distribuyendo la disyunción de manera que las
  disyunciones queden adentro de los parentesis y el operador principal sea
  la conjunción.
-}
distribuyeDisy :: Prop -> Prop
distrubuyeDisy (Var p) =  p
distribuyeDisy (Disy (Conj p r) s) =
    distribuyeDisy(Conj (Disy (distribuyeDisy p) (distribuyeDisy s))(Disy (distribuyeDisy r) (distribuyeDisy s)))
distribuyeDisy (Disy p (Conj r s)) = 
    distribuyeDisy(Conj (Disy (distribuyeDisy p) (distribuyeDisy r))(Disy (distribuyeDisy p) (distribuyeDisy s)))
distribuyeDisy (Conj p r) =
    Conj (distribuyeDisy p) (distribuyeDisy r)
distribuyeDisy (Disy p r)
    | hayConj (Disy p r) = distribuyeDisy(Disy (distribuyeDisy p)(distribuyeDisy r))
    | otherwise = Disy (distribuyeDisy p)(distribuyeDisy r)



{-
Función hayConj: Recibe una Prop y regresa un booleano, True en caso de que
la expresión tenga conjunciones, False en caso contrario. 
-}
hayConj:: Prop -> Bool
hayConj (Conj p r) = True
hayConj (Disy p r) = (hayConj p) || (hayConj r)
hayConj _          = False


{-
  Función fnn: Recibe una Prop (expresión de la lógica proposicional)
  y regresa su forma normal negativa para ya no lidiar con las implicaciones,
  equivalencias y dobles negaciones. 
-}
fnn :: Prop -> Prop
fnn f = simplify(deleteImpl f)


{-
Funcion aux: Cuenta el numero de apariciones del elemento en la lista de nodos.
-}
aux :: String -> [(String,String)]-> Int
aux _[] = 0
aux e ((x1,x2):xs)
  |e == x1 || e== x2 = 1 + aux e xs
  |otherwise = aux e xs
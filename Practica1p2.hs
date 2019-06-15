module Practica1p2 where

{-|1|
   Función permutaciones: Recibe una lista de elementos y devuelve todas 
   las permutaciones de los elementos de la lista.
   Hint: Se recomienda el uso de la función intercala.
-}
permutaciones:: [a] -> [[a]]
permutaciones  [] = [[]]
permutaciones (x:xs) = concat [intercala x xs | xs <- permutaciones xs]


-- |2| Función factores: Recibe un entero y regresa la lista de sus factores.
factores :: Int -> [Int]
factores n = 
    if n > 0 then [x | x <- [1..n], mod n x == 0]
    else error "No se puede definir factores de numeros negativos"

{-|3| Función perfectos: Recibe un número n y devuelve la lista de números 
perfectos que se encuentran hasta el número n. 
Por definición, un numero perfecto es un entero positivo que es igual a la suma
de sus divisores propios.
-}
perfectos :: Int -> [Int]
perfectos 0 = error "indefinidos"
perfectos n = 
    if n > 0 then [x | x <- [1..n], esPerf x]
    else error "No se puede definir numeros perfectos de numeros negativos"

{-|4| Función ternasPitagoricas: Recibe un número y regresa la lista de ternas 
pitagóricas que correspondan.
-}
ternasPitagoricas:: Int -> [(Int,Int,Int)]
ternasPitagoricas 0 = [(0,0,0)]
ternasPitagoricas x =
    if x > 0 then [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]
    else error "No se puede definir ternas pitagóricas de numeros negativos"


{-|5| Función isSubSet: Recibe dos listas y nos dice si la primera lista es 
subconjunto de la segunda, si lo es regresa True. En otro caso, regresa False.
-}
isSubSet :: Eq a => [a] -> [a] -> Bool
isSubSet []     _ = True
isSubSet (x:xs) ys = elem x ys && isSubSet xs ys

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)

leaf x = Branch x Empty Empty

{-|6| Función deleteT: Elimina el elemento de un árbol binario.
Recibe un elemento y un árbol. Regresa el arbol sin el elemento.
Si el árbol es vacío se regresa el árbol vacío.
Si el hijo izquierdo es vacío se regresa el hijo derecho.
Si el hijo derecho es vacio se regresa el hijo izquierdo
Si el elemento es menor al elemento de la raiz hace recursión sobre el hijo izquierdo
Si el elemento es mayor al elemento de la raiz hace recursión sobre el hijo derecho
Por último, si el elemento es la raíz, se busca el elemento más grande del hijo izquierdo
y luego se elimina.
-}
deleteT :: (Eq a, Ord a) => a -> Tree a -> Tree a
deleteT _ Empty = Empty
deleteT n (Branch raiz izq der) 
          | izq == Empty = der
          | der == Empty = izq
          | n < raiz = (Branch raiz (deleteT raiz izq) der)
          | n > raiz = (Branch raiz izq (deleteT n der))
          | otherwise = (Branch raizX izqX der)
            where raizX = treeMax izq
                  izqX = deleteT raizX izq        

{- |7| Función balanced: Recibe un árbol. Nos dice si un árbol binario esta balanceado,
si lo es regresa True. En otro caso, regresa False.
Un árbol binario balanceado es un árbol binario en el cual las alturas 
de los dos subárboles de todo nodo difiere a lo sumo en 1.
-}
balanced :: Tree a -> Bool
balanced Empty = True
balanced (Branch raiz izq der)
          | profundidad izq > profundidad der && profundidad izq -1 == profundidad der = True
          | profundidad der > profundidad izq && profundidad der -1 == profundidad izq = True
          | otherwise = False


{- |8| Función pre: Recibe un árbol. Devuelve el resultado de recorrer un arbol en pre-orden.
Para hacer el recorrido de un árbol en preorden, primero se visita la raíz, luego se hace
la llamada recusiva sobre el hijo izquierdo y cuando termine de procesarse, se hace la 
llamada recursiva sobre el hijo derecho.
-}
pre :: Tree a -> [a]
pre Empty = []
pre (Branch raiz izq der) = (raiz: pre izq ++ pre der)

-- Funciones auxiliares

{- Función intercala: Recibe un elemento y una lista de elementos. 
   Devuelve la lista de listas con el elemento intercalado en una lista.
-}
intercala :: a -> [a] -> [[a]]
intercala a [] = [[a]]
intercala a (x:xs) = (a:x:xs) : [(x:xs) | xs <- intercala a xs]

{- Función profundidad: Recibe un árbol binario. Devuelve la profundidad del arbol binario.
   Por definición, la profundidad de un arbol binario es 1 más el máximo de su hijo
   izquierdo o derecho. 
-}
profundidad :: Tree a -> Int
profundidad Empty = 0
profundidad (Branch raiz izq der) = 1 + max (profundidad izq) (profundidad der)

{-
  Funcion de divisoresPropios: Recibe un entero positivo y te devuelve la lista con sus 
  divisores propios.
-}
divisoresPropios:: Int -> [Int]
divisoresPropios n = 
  if n >= 0 then [x | x <- [1..(n-1)],(n `mod` x)==0] else error "no puedes sacar divisiores propios de un numero menor que cero"

{-
  Funcion esPerf: Recibe un entero positivo. Devuelve True si es numero perfecto. En otro caso devuelve False.
-}
esPerf:: Int -> Bool
esPerf n = if n >= 0 then (n == sum(divisoresPropios n)) else error "No hay numeros perfectos negativos"


{-
  Función treeMax: Recibe un arbol y regresa el elemento más grande del árbol.
  Por definición, un arbol binario ordenado es aquél donde el hijo derecho es mayor al 
  hijo izquierdo de cualquier nodo.
-}
treeMax :: (Eq a) => Tree a -> a
treeMax (Branch raiz izq der) = if der /= Empty then treeMax der else raiz
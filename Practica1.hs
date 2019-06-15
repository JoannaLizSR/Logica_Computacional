module Practica1 where

{- |1| Función getNth: Recibe un número y una lista. Regresa el n-ésimo 
   elemento de la lista. 
-}   
getNth :: Int -> [a] -> a
getNth 0 (x:_) = x
getNth _ [] = error "errror"
getNth n (_:xs) 
            | n<0 = error "Valor no valido"
            | otherwise = getNth (n-1) xs 

{- |2| Función update: Recibe un número, un elemento y una lista. Actualiza 
   el n-ésimo elemento de la lista.
-}
update :: Int -> a -> [a] -> [a]
update _ a [] = []
update 0 a (x:xs) = (a:xs)
update n a (x:xs) = (x:(update (n-1) a (xs)))

{- |3| Función dropP: Recibe un número entero y uan lista. Regresa la lista
   sin los elementos de cada n posición. 
   Se prohíbe el uso de las funciónes TAKE y DROP.
-}
dropP :: Int -> [a] -> [a]
dropP n (x:xs) =  dropAux n n (x:xs)

{-
   Función dropAux : Recibe dos números y una lista. Borra el elemento en 
   la m posición y luego cada n posición. Regresa la lista sin los elementos 
   en la m posición y cada n posición.
-}
dropAux :: Int -> Int -> [a] -> [a]
dropAux n m [] = []
dropAux n m (x:xs)
                | m == 1 = dropAux n n xs
                | otherwise = x:(dropAux n (m - 1) xs)  

{- |4| Función split: Recibe un número y una lista. Separa una lista en dos 
   partes, tomando en cuenta el entero dado. Se obtiene una lista de listas.
   Se prohíbe el uso de las funciónes TAKE y DROP.
   Hint: Usar las funciones takeL y dropL vistas en el laboratorio.
-}
split :: Int -> [a] -> [[a]]
split _ [] = []
split n (x:xs) = takeL n (x:xs) : split n (dropL n (x:xs)) 

{- |5| Función range: Recibe dos números enteros. Regresa los números que
   están entre el rango de n y m. Si n y m son iguales, regresa la lista
   con el número n. Si n es mayor a m, entonces se regresa una lista con n 
   como cabeza y cada vez que entre la llamada recursiva se disminuye a n
   en 1 para que el antecesor de n sea la nueva cabeza y así sucesivamente 
   hasta llegar a m. Finalmente si n es menor a m, se regresa una lista con n
   como cabeza y cada vez que entre la llamada recursiva se aumenta a n en 1 
   para que el sucesor de n sea la nueva cabeza y así sucesivamente hasta 
   llegar a m. 
-}
range :: Int -> Int -> [Int]
range n m 
            | n == m = [n]
            | n > m = n : range(n-1) m
            | otherwise = n : range(n+1) m

{- |6| Función filterP: Regresa los elementos de una lista que cumplen un 
   predicado.  Se prohíbe el uso de la función FILTER.
-}
filterP :: (a -> Bool) -> [a] -> [a]
filterP _ [] = []
filterP t (x:xs) 
            | t x = x : filterP t xs
            | otherwise = filterP t xs


{-|7| Función rotate: Rota n lugares de una lista.-}
rotate :: Int -> [a] -> [a]
rotate 0 l = l
rotate n (x:xs) = rotate (n-1) (xs ++ [x])

{- |8| Función palindrome: Nos dice si una lista es palindroma o no. Por
   definición, una palabra palíndroma es la lista vacía y la lista con un
   elemento. Si la lista es igual a su reversa, entonces regresa True.
   En caso contrario, regresa False.
-} 
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) =
    if (headlL (x:xs) == lastL (x:xs)) then palindrome (initL (tailL (x:xs)))  
    else False

{- |9| Función intercala: Recibe un elemento y una lista de elementos. 
   Devuelve la lista de listas con el elemento intercalado en una lista.
-}
intercala :: a -> [a] -> [[a]]
intercala a [] = [[a]]
intercala a (x:xs) = (a:x:xs) : [(x:xs) | xs <- intercala a xs]
 
{- |10| Función factorial: Recibe un número. Si el número es negativo, regresa
   un error. En caso contrario, regresa el factorial del número.
-} 
factorial :: Int -> Int
factorial 0 = 1
factorial n = 
    if n < 0 then error "El número debe ser un entero positvo." 
    else n*factorial (n-1)  

{- |11| Función repite: Recibe un número y un elemento. Si el número es
   negativo, regresa un error. En caso contrario, regresa una lista con el
   elemento repetivo n veces.
-}
repite :: Int -> a -> [a] 
repite 0 a = []
repite n a = 
    if n < 0 then error "El número debe ser un entero positivo." 
    else (a:repite (n-1) a)

{- |12| Función fib: Recibe un número entero. Regresa la función fibonacci 
   aplicada al número.
 -}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n =
    if n < 0 then error "El número debe ser un entero positivo."
    else fib (n-1) + fib(n-2)

-- Funciones auxiliares.

-- Función isEmpty: Recibe una lista. Nos dice si la lista es la lista vacía.
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

{- Función takeL: Recibe un número entero y una lista. Toma los primeros n 
   elementos de una lista.
-}
takeL :: Int -> [a] -> [a]
takeL  0 _ = []
takeL _ [] =[]
takeL n (x:xs) = x:takeL (n-1) xs

{- Función dropL: Recibe un número entero y una lista. Elimina los primeros n 
   elementos de una lista.
-}
dropL :: Int -> [a] -> [a]
dropL _ [] = []
dropL 0 (x:xs) = x:xs  
dropL n (x:xs) = dropL (n-1) xs  

-- Función headL: Recibe una lista y regresa el primer elemento de la lista.
headlL :: [a] -> a
headlL [] = error "La lista está vacía."
headlL (x:xs) = x

-- Función lastL: Recibe una lista y regresa el úlitmo elemento de la lista.
lastL :: [a] -> a
lastL [] = error "La lista está vacía."
lastL (x:[]) = x
lastL (x:xs) = lastL xs 

-- Función tailL: Recibe una lista y regresa la cola de la lista.
tailL :: [a] -> [a]
tailL [] = error ""
tailL (x:xs) = xs 

{- Función initL: Recibe una lista. Si la lista recibida es vacía, entonces
   regresa la lista vacía. En caso contrario, regresa la lista sin el último
   elemento. 
-}
initL :: [a] -> [a]
initL [] = error " D="
initL (x:xs) = if (isEmpty xs) then [] else x:(initL xs)

-- Función lengthL: Recibe una lista y regresa el tamaño de la lista.
lengthL :: [a] -> Int
lengthL [] = 0
lengthL (x:xs) = 1 + lengthL (xs)
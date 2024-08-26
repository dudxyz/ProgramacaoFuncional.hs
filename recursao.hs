import Data.Char
-- A Função de or.
ou :: [Bool] -> Bool
ou [] = False 
ou (x:xs) = x || ou xs 

-- 1. O produto dos elementos de uma lista de inteiros.
produto :: [Int] -> Int 
produto [] = 1
produto (y:ys) = y * produto ys 

-- 2. Filtrar os números ímpares, ou seja, da lista original, ficar somente com os ímpares.
impares :: [Int] -> [Int]
impares [] = []
impares (a:as)
 |odd a = a : impares as 
 |otherwise = impares as

-- 3. Triplicar todos os elementos de uma lista de inteiros.
triplica :: [Int] -> [Int]
triplica [] = []
triplica (b:bs) = b*3 : triplica bs

-- 4. Triplicar somente os pares, os outros se mantém intatos.
triplicaPar :: [Int] -> [Int]
triplicaPar [] = []
triplicaPar (c:cs)
 |even c = c*3 : triplicaPar cs
 |otherwise = c : triplicaPar cs

-- 5. Triplicar somente os pares, os ímpares serão eliminados.
triplicaElimina :: [Int] -> [Int]
triplicaElimina [] = []
triplicaElimina (d:ds) 
 |even d = d*3 : triplicaElimina ds
 |otherwise = triplicaElimina ds

-- 6. Verificar se uma string contém algum caractere alfabético (letras, acentuada ou não).
charAlpha :: String -> Bool
charAlpha [] = True
charAlpha (e:es)  
 |isAlpha e = isAlpha e && charAlpha es
 |otherwise = False 

-- 7. Eliminar a primeira ocorrência de um dado elemento, se ele ocorrer, senão retornar a lista original.
removePrimeiro :: Int -> [Int] -> [Int]
removePrimeiro _ [] = []
removePrimeiro x (y:ys) 
       |x == y = ys
       |otherwise = y : removePrimeiro x ys
-- 8. Eliminar todas as ocorrências de um dado elemento.
removerElem :: Int -> [Int] -> [Int]
removerElem _ [] = []
removerElem  x (y:ys) 
    |x == y = removerElem x ys
    |otherwise = y : removerElem x ys

-- 9. Inverter uma lista de inteiros.
inverter :: [Int] -> [Int]
inverter [] = []
inverter (x:xs) = inverter xs ++ [x]

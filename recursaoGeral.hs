main = return ()

-- 1. Sem usar mod, defina uma função meuMod que faça o mesmo que mod.
meuMod :: Int -> Int -> Int
meuMod x 0 = 0
meuMod x 1 = 0
meuMod x y 
  |x<y = x  
  |otherwise = meuMod (x-y) y

-- 2. Sem usar nem div nem mod, defina uma função chamada divMod que calcula ao mesmo tempo a divisão inteira e o resto da divisão inteira (retorna uma dupla).
meuDivMod :: Int -> Int -> (Int,Int)
meuDivMod x y = (meuDiv x y, mMod x y)

meuDiv :: Int -> Int -> Int
meuDiv x 1 = x
meuDiv x y 
  |x < y = 0
  |otherwise = 1 + meuDiv (x-y) y

mMod :: Int -> Int -> Int
mMod x 0 = error "Infinito"
mMod x 1 = 0
mMod x y 
  |x<y = x  
  |otherwise = mMod (x-y) y

-- 3. Usar esta propriedade para dar uma definição recursiva para a função mdc.
mdc :: Int -> Int -> Int
mdc x 0 = x
mdc x y
  |x > y = mdc (mod x y) y
  |mod x y == 0 = y
  |otherwise = 0

-- 4. Considere a definição de myTake vista em aula. O que acontece quando o primeiro argumento é negativo? Redefina myTake de tal forma que myTake n xs retorne [] quando n < 0.
myTake :: Int -> [Int] -> [Int]
myTake n [] = []
myTake 0 xs = []
myTake n (x:xs)
 |n<0 = []
 |otherwise = x : myTake (n-1) xs
 
-- 5. Defina a função myDrop que faz o mesmo que a função do Prelude drop. Tenha cuidado para que myDrop n xs retorne xs quando n < 0.
myDrop :: Int -> [Int] -> [Int]
myDrop n [] = []
myDrop 0 xs = xs
myDrop n (x:xs) 
 |n<0 = xs 
 |otherwise = myDrop (n-1)  xs

-- 6. Defina a função myLast que retorna o último elemento de uma lista. 
myLast :: [Int] -> [Int]
myLast [] = error "Lista vazia"
myLast [x] = [x]
myLast (x:xs) = myLast xs

-- 7. Defina a função myInit que retorna todos os elementos de uma lista menos o último. 
myInit :: [Int] -> [Int]
myInit [] = error "Lista vazia"
myInit [x] = []
myInit (x:xs) = x : myInit xs

-- 8. Sem usar ranges, defina a função enumereDeAte tal que enumereDeAte m n seja igual com o range [m..n]. 
enumereDeAte :: Int -> Int -> [Int]
enumereDeAte x 0 = []
enumereDeAte x y 
 |x > y = []
 |otherwise = x : enumereDeAte (x + 1) y

-- 9.  Sem usar ranges, defina a função enumereDeEntaoAte tal que enumereDeEntaoAte m n p seja igual com o range [m,n..p].
enumereDeEntaoAte :: Int -> Int -> Int -> [Int]
enumereDeEntaoAte x n 0 = []
enumereDeEntaoAte x n y 
 |y < x = []
 |otherwise = x : enumereDeEntaoAte n (n+n) y

-- 10. Defina a função filtraPosicoesPares que retorna todos os elementos da lista de entrada que estão em posições pares.
filtraPosicoesPares :: [Int] -> [Int] 
filtraPosicoesPares [] = []
filtraPosicoesPares [x] = [x]
filtraPosicoesPares (x:x2:xs) = x : filtraPosicoesPares xs

-- 11. Defina a função filtraPosicoesImpares que retorna todos os elementos da lista de entrada que estão em posições ímpares.
filtraPosicoesImpares :: [Int] -> [Int]
filtraPosicoesImpares [] = []
filtraPosicoesImpares [x] = []
filtraPosicoesImpares (x:x2:xs) = x2 : filtraPosicoesImpares xs

-- 12. Definir as funções filtraPosicoesPares filtraPosicoesImpares usando recursão mútua.
filtraPosicoesPares2 :: [Int] -> [Int] 
filtraPosicoesPares2 [] = []
filtraPosicoesPares2 [x] = [x]
filtraPosicoesPares2 (x:x2:xs) = x : filtraPosicoesImpares2 (x2 : filtraPosicoesImpares2 xs)

filtraPosicoesImpares2 :: [Int] -> [Int]
filtraPosicoesImpares2 [] = []
filtraPosicoesImpares2 [x] = []
filtraPosicoesImpares2 (x:x2:xs) = x2 : filtraPosicoesPares2 xs

-- 13. Considere a seguinte série 0 1 2 2 3 5 7 10 15 22 32 ... Defina uma função que, dado n, retorne o n-ésimo elemento da série.
serie :: Int -> Int
serie 0 = 0
serie 1 = 1
serie 2 = 2
serie n = serie (n-1) + serie (n-3)

-- 14. Defina a função myZip que recebe duas listas xs e ys e retorna uma lista de duplas. Na lista resultante, a primeira dupla é formada com ambos os primeiros elementos de xs e ys, a segunda dupla com os segundos elementos de xs e ys e assim por diante. Se uma das listas xs ou ys tem mais elementos que a outra, os elementos que sobram são ignorados. Assim, por exemplo myZip [2,3,4] ['a', 'b', 'c', 'd'] = [(2, 'a'), (3, 'b'),(4,'c')]
myZip :: [Int] -> [Int] -> [(Int, Int)]
myZip xs [] = []
myZip [] ys = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 15. Defina a função myUnzip que faz o inverso de myZip. Assim, por exemplo myUnzip [(2, 'a'), (3, 'b'),(4,'d')] = ([2,3,4], ['a', 'b', 'c']).
myUnzip :: [(Int, Int)] -> ([Int], [Int])
myUnzip [] = ([], [])
myUnzip ((x,y):resto) = (x:xs, y:ys)
  where 
   (xs,ys) = myUnzip resto 
   

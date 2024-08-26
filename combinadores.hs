import Data.Char
main = return ()

--1. 
quadrados :: Num a => [a] -> [a]
quadrados as = map quadrado as 
 where 
  quadrado a = a * a

--2. 
segundoElem :: [(a,b)] -> [b] 
segundoElem as = map snd as

--3.
comprimento :: [Int] -> Int
comprimento as = sum (map paraum as)
 where
  paraum a = 1

--4.a)
dobroLista :: [Int] -> [Int]
dobroLista as = zipWith (+) as as

--4.b)
quadradoLista :: [Int] -> [Int]
quadradoLista bs = zipWith (*) bs bs

--5.
ordenada :: [Int] -> Bool
ordenada [] = True
ordenada xs = and (zipWith (<=) xs (tail xs))

--6. 
temDigito :: String -> Bool
temDigito cs
 |any isDigit cs  = True
 |otherwise      = False

--7. 
temChar :: String -> Bool
temChar cs
 |any isAlpha cs  = True
 |otherwise       = False

--8. 
naoTemChar :: String -> Bool
naoTemChar cs
 |any isAlpha cs = False
 |otherwise      = True

--9. 
positivos :: Num a => Ord a => [a] -> [a]
positivos as = filter ePositivo as
 where
  ePositivo a
   |a >= 0    = True
   |otherwise = False

--10.
parQuadrado :: [(Int, Int)] -> [(Int ,Int)]
parQuadrado as = map quadrado as
 where
  quadrado (a, b) = (a, b^2)

--11.
posicoes :: Int -> [Int] -> [Int]
posicoes v as = map snd (filter ocorrencias (zip as [0..]))
  where
   ocorrencias (a,v) = a == v 

--12.
ultimaPosicao :: [Int] -> Int -> Int
ultimaPosicao [] _ = -1
ultimaPosicao xs v
 |last res@(map snd (filter ocorrencias posicoes)) /= [] = res
 |otherwise = -1
  where 
   posicoes = zip xs [0..]
   ocorrencias (a,b) = a==v

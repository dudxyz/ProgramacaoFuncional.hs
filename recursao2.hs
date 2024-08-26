main = return ()

-- 1. Defina a função multiplica que multiplica dois números. Não pode usar o operador (*), mas pode usar (+).
multiplique :: Int -> Int -> Int 
multiplique x 0 = 0
multiplique x 1 = x 
multiplique x y = x + multiplique x (y-1)

-- 2. Escreva uma função que receba um inteiro m e um natural n e calcule m . Não pode n usar os operadores (**) e (^).
exponencial :: Int -> Int -> Int
exponencial m 0 = 1
exponencial m 1 = m
exponencial m n = m * exponencial m (n-1)

-- 3. Escreva uma função que calcule a soma dos cubos de 1..n.{-# INLINABLE function #-}
somaCubos :: Int -> Int
somaCubos 0 = 0
somaCubos 1 = 1
somaCubos x = x^3 + somaCubos (x-1)

-- 4. Escreva uma função que calcule os exponeciais (n) de 2.
expoDois :: Int -> Int
expoDois 0 = 1
expoDois x = 2^x + expoDois (x-1)

-- 5. Escreva uma função que calcule: 1/0! + 1/1! + 1/2! +... + 1/n! Só como informação, esta série permite calcular aproximações para a constante de euler e. Em particular, o limite desta série é e.
fatorial :: Int -> Int -- Definindo uma função para o fatorial.
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

euler :: Int -> Float
euler 0 = 1
euler x = 1/fromIntegral (fatorial x) + euler (x-1)

-- 6. Escreva uma função que, dado um natural n, calcule a raiz quadrada inteira de n, ou seja, calcule o maior natural cujo quadrado é menor ou igual a n.
raizInteira :: Int -> Int
raizInteira 0 = 0
raizInteira x = procura x 1
  where 
   procura x n  -- função para procurar uma raiz aproximada de x
    |n*n > x = (n-1)
    |n*n == x = n
    |otherwise = procura x (n+1)

-- 7. Defina uma função que aceite um natural n e devolva o maior valor dentre f 0, f 1, f 2, ... f n.
maiorValor :: Int -> Int
maiorValor 0 = f 0
maiorValor n
 |f n >= maiorValor (n-1) = f n
 |otherwise = maiorValor (n-1)

f :: Int -> Int
f 0 = 8
f 1 = 44
f 2 = 17
f 3 = 2
f _ = 0

-- 8. Defina uma função que aceite um natural n e devolva True se e somente se um ou mais valores dentre f 0, f 1, f 2, ... f n é zero.
eZero :: Int -> Bool
eZero n 
 |n == 0 = f 0 == 0
 |f n == 0 = True
 |otherwise = eZero (n-1)

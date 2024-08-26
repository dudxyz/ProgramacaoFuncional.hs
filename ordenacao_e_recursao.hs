import Data.Char
main = return ()

-- 1. Defina a função merge de tal maneira que ao receber duas listas ordenadas xs e ys, merge xs ys retorna todos os elementos de xs e ys numa única lista, também ordenada.
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 |x <= y    = x : merge xs (y:ys)
 |otherwise = y : merge (x:xs) ys

 -- 2. Usando merge, defina a função mergeSort que ordena a lista de entrada. A ideia é dividir a lista de entrada em duas partes de tamanho igual ou no máximo variando em um, recursivamente ordenar cada parte e então aplicar merge.
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort xs1) (mergeSort xs2)
 where
  (xs1, xs2) = splitAt meio xs
  meio = div (length xs) 2 
-- 3. Dadas duas listas ordenadas ascendentemente, cada uma sem elementos repetidos, defina funções para calcular a união e a interseção. Os resultados devem ser listas ordenadas sem elementos repetidos. Não pode usar nenhuma função pré-definida. Em particular, não pode usar elem e nem definir uma função que faça o mesmo que ela.
uniaoInter :: [Int] -> [Int] -> ([Int], [Int])
uniaoInter xs ys = (uniao xs ys, inter xs ys)

uniao :: [Int] -> [Int] -> [Int]
uniao xs [] = xs
uniao [] ys = ys
uniao (x:xs) (y:ys) 
  |x < y = x : uniao xs (y:ys)
  |y < x = y : uniao ys (x:xs)
  |otherwise = x : uniao xs ys

inter :: [Int] -> [Int] -> [Int]
inter xs [] = []
inter [] ys = []
inter (x:xs) (y:ys) 
  |x == y = x : inter xs ys
  |otherwise = inter xs (y:ys)

-- 4. Utilizando somente as funções chr e ord do módulo Data.Char, definir uma função que transforme um número natural para string.
chrOrd :: [Int] -> [Char]
chrOrd [] = []
chrOrd (x:xs) 
  |x >= 0 && x <= 9 = paraChar x : chrOrd xs
  |x >= 10 && x <= 99 = paraChar (div x 10) : paraChar (mod x 10) : chrOrd xs
  |otherwise = chrOrd xs
    where
     paraChar x = chr (ord '0' + x)

-- 5. Defina uma função que calcule a soma de todos os dígitos de um número. Por exemplo, para o número 2315 a função deverá retornar 2+3+1+5, ou seja 11.
somaDigito :: Int -> Int 
somaDigito 0 = 0
somaDigito x 
  |x >= 0 && x <= 9 = x
  |x >= 10 = mod x 10 + somaDigito (div x 10)
  |otherwise = somaDigito x
  
-- 6. Em uma aula anterior fizemos um exercício para formatar uma conta de supermercado. Para isto, tínhamos definido os seguintes tipos Sem usar compreensões nem nenhuma função pré-definida, redefina a função produzirConta. Antes, defina as funções preco e nome, e use-as na definição de produzirConta. Todas suas definições deverão ser recursivas.
type Codigo = Int
type Nome = String
type Preco = Int
type Compras = [Codigo]
type BancoProdutos = [(Codigo, Nome, Preco)]
type Conta = [(Nome, Preco)]

nome :: Codigo -> BancoProdutos -> Nome 
nome _ [] = " "
nome c ((cod, nm, pc):xs)
  |c == cod = nm
  |otherwise = nome c xs

preco :: Codigo -> BancoProdutos -> Preco
preco _ [] = 0
preco c ((cod, nm, pc):xs)
  |c == cod = pc
  |otherwise = preco c xs

produzirConta :: Compras -> BancoProdutos -> Conta 
produzirConta [] _ = []
produzirConta (c:cps) ((cod, nm, pc):xs) = (n,p) : produzirConta cps xs
  where 
   n = nome c ((cod, nm, pc):xs)
   p = preco c ((cod, nm, pc):xs)
   
bancoProdutosExemplo :: BancoProdutos
bancoProdutosExemplo = 
    [(1, "Maca", 150),
    (2, "Banana", 100),
    (3, "Laranja", 120),
    (4, "Pera", 180)]

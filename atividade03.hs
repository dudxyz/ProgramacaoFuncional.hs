import Data.Char
main = return ()

-- 1. Defina uma função que dada uma lista de inteiros retorna os ímpares desta lista, porém triplicados.
impares :: [Int] -> [Int]
impares ns = [3*n|n <- ns, odd n]

-- 2. Defina uma funçõa que dada uma lista de inteiros retorna uma lista com os impares triplicados e os pares inalterados.
imparesTriplicados :: [Int] -> [Int]
imparesTriplicados ns = [if odd n then n else n*3|n<-ns]

-- 3. Defina uma função que permita verificar se um dado inteiro está dentro de uma lista. (Não pode usar elem).
verifiqueInt :: Int -> [Int] -> Bool
verifiqueInt n ns = [x| x<-ns, x==n] /= []

-- 4. Defina uma função que dado um string e um inteiro n, construa a lista com este string repetido n vezes.
listaString :: String -> Int -> [String]
listaString str n = [str| _ <-[1..n]]

-- 5. Defina uma função que dada uma string, devolve a string em que todo caractere é transformado para maiúsculo. Utilize a função toUpper do módulo Data.Char.
cM :: String -> String
cM xs = [toUpper x|x<-xs]

-- 6. Defina uma função que dada uma string, retorna a string eliminando todos os caracteres que são dígitos. Utilize a função isDigit do módulo Data.Char.
eliminado :: String -> String
eliminado xs = [x|x<-xs, not (isDigit x)]

-- 7. Defina uma função para verificar se algum elemento de uma lista dada é par.
elementoPar :: [Int] -> Bool
elementoPar xs = [x|x<-xs, even x] /= []

-- 8. Defina uma função que permita calcular o máximo divisor comum de dois números. Se precisar, pode usar as funções max e min que calculam o maior e o menor dentre dois números, respectivamente. Também pode precisar usar a função last que retorna o último elemento de uma lista.
mdc :: Int -> Int -> Int
mdc x y = last [z|z<-[1..(max x y)], x `mod` z == 0 && y `mod` z == 0]

-- 9. Defina uma função que, dadas duas listas de palavras, sendo uma de adjetivos e outra de nomes, construa todas as combinações possíveis de nomes com adjetivos.
combinacoes :: [String] -> [String] -> [String]
combinacoes adj nom = [a++ " " ++ n| a<-adj, n<-nom]

-- 10. Defina uma função que dada uma lista de strings retorna a string obtida pela concatenação de todos os strings na lista. Dica: utilize compreensão com dois geradores.
concatenaStr :: [String] -> String
concatenaStr xss = [ x | xs <- xss, x <- xs]

-- 11. Defina uma função que receba uma lista de strings e que retorne um string que quando impresso na tela com putStr mostre cada string da lista em uma linha separada.
separaStr :: [String] -> String
separaStr strs = concatenaStr [str ++ "\n"| str<-strs]

-- 12. Defina uma função que dado um número n, produza a tabela de multiplicação do n. Assim, por exemplo, se n=3, a função produzirá um string que, ao apresentar na tela, seguirá este padrão. Talvez precise da função show para transformar inteiros para String.
tabelaDo :: Int -> String
tabelaDo m = cabecalho ++ "\n" ++ tabela ++ "\n"
  where
   cabecalho = "n   " ++ show m ++ "xn"
   tabela = concatenaStr [ show i ++ "   " ++ show (i*m) ++ "\n" | i <- [1..10]]

-- 13. (5.27)
type Pessoa = String
type Livro = String
type DataBase = [(Pessoa, Livro)]
baseDados :: DataBase -> Pessoa -> [Livro]
baseDados db p = [l| (n, l) <- db, p == n]

db :: DataBase
db = [("Ana", "Moby Dick"),
      ("Joao","1987"), ("Joao", "Haskell")]

-- (5.28) 
numeroEmprestado :: DataBase -> Pessoa -> Int
numeroEmprestado db p = length (baseDados db p)

-- (5.29)
retorno :: DataBase -> Pessoa -> Livro -> DataBase
retorno db p l = [em|em<-db, em /= (p, l)]

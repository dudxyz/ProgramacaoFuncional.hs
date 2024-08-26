import Data.Char
main = return ()

-- 1. Dado n, imprimir a tabuada de multiplicação do n. A função deverá retornar uma string que ao ser passada para putStr escreva na tela a tabuada do n.
tabuada :: Int -> String 
tabuada n = tab 10
 where
  tab 1 = "1x" ++ show n ++ "=" ++ show n ++ "\n"
  tab m = tab (m-1) ++ "x" ++ show n ++ "=" ++ show (m*n) ++ "\n"

-- 3. Dada uma lista de pares (Nota, NomeAluno), calcular os nomes daqueles que têm nota acima da média.
type NomeAluno = String
type Nota = Double

nomeAcima :: [(Nota,NomeAluno)] -> [NomeAluno]
nomeAcima [] = []
nomeAcima ((a,b):xs)
 | a > media = b : nomeAcima xs
 | otherwise = nomeAcima xs
  where
     media = somaA/ fromIntegral (length ((a,b):xs))
     somaA = sum (fst(unzip((a,b):xs)))

-- 5. Resolva o problema de encontrar o máximo divisor comum de dois inteiros positivos a e b generalizando o problema para outro que, dentre os números que estão no intervalo entre 1 e m, encontre o maior divisor comum de a e b.
mdc :: Int -> Int -> Int
mdc a b = mdcAux (min a b)
 where
   mdcAux m 
    | mod a m == 0 && mod b m == 0 = m
    | otherwise = mdcAux (m-1)

-- 7. Melhore a eficiência da função anterior utilizando a seguinte ideia. Se m é o maior entre m e n, o mmc de m e n é o primeiro múltiplo de n dentre os múltiplos de m.
mmc :: Int -> Int -> Int
mmc a b = mmcAux (max a b)
  where
   mmcAux m
    | mod m (min a b) == 0 = m
    | otherwise = mmcAux (m + (max a b))
    
-- 8. Considere a série 0 1 2 2 3 5 7 10 15 22 32. Dê uma definição eficiente para calcular o n-ésimo número da série. A função recebe como argumento o valor de n.
serie :: Int -> Int
serie n = serieAux n 0 1 2 2 
 where
 serieAux 0 a _ _ _ = a
 serieAux 1 _ b _ _ = b
 serieAux 2 _ b _ _ = b
 serieAux 3 _ _ c _ = c
 serieAux n a b c d = serieAux (n - 1)  b  c  d (a+b+c)

-- 9. Defina uma função que avalie a série i−0 n ∑ 1/i!. A função recebe n como argumento. Use a técnica de pedir mais informações ao amigo para não ter que fazer o cálculo completo do fatorial para cada um dos termos da série.
eAp :: Double -> Double 
eAp m = fst (eAp' m)
 where
 eAp' 0 = (1,1)
 eAp' n = (s+t/n, t/n)
   where
   (s,t) = eAp' (n-1)
   
-- 10. Uma subsequência de um string st é uma sequência que está contida dentro de st, com todos seus elementos ineterruptos. Dada uma String st, calcular o tamanho da maior subsequência de vogais. Como informação extra, peça ao amigo o tamanho do maior prefixo de st formado só por vogais.
sv :: String -> (Int,Int) 
sv "" = (0,0)
sv (c:cs) 
 | eVogal c && p == s = (p+1,p+1)
 | eVogal c = (s,p+1)
 | otherwise = (s,0)
  where
   (s,p) = sv cs  
   
eVogal :: Char -> Bool
eVogal c = elem c "aeiouAEIOU" 

-- 12. Calcular o fatorial de um natural.
fatorial :: Int -> Int
fatorial n = fatAux n 1
  where
  fatAux 0 res = res 
  fatAux n res = fatAux (n-1) (n*res)
  
-- 13. Calcular m^n.
potencia :: Int -> Int -> Int
potencia m n = potAux m n 1
 where
 potAux m 0 res = res 
 potAux m n res = potAux m (n-1) (res*m)

-- Pode cair: 5. queremos encontrar [n, p .. m]. Obs: nessa questão, chamamos de "passo" o resultado de p - n. Por exemplo: para [4, 8 .. 16], o passo = 8 - 4 = 4 para [16, 12 .. 4], passo = 12 - 16 = -4. Caso base: se o passo for negativo e p já for menor que o (m + passo), então retornamos a lista vazia. O raciocínio é similar para passo positivo.HI: sei encontrar [p, p + passo .. m]. caso geral: faço n : [p, p + passo .. m]
myRange n p m
  | (passo < 0 && p < (m + passo)) || -- casos base.
    (passo > 0 && p > (m + passo)) = []
  | otherwise = n : myRange p p' m -- passo indutivo e HI.
  where
    passo = p - n
    p' = p + passo

main :: IO
main = do
-- 01.Defina uma função que receba três números e determine se todos são diferentes.
numDif :: Int -> Int -> Int -> String
numDif a b c
   |a == b && b == c = "Iguais"
   |otherwise        = "Diferentes"
-- 02.Defina uma função que receba três números e determine o menor deles.
menor3 :: Int -> Int -> Int -> Int
menor3 a b c
   |a < b && a < c = a 
   |b < a && b < c = b 
   |otherwise      = c
-- 03.Defina uma função que receba três números e determine quantos estão acima da média dos três. 
acimaMedia :: Float -> Float -> Float -> Int
acimaMedia a b c
  |a == b && b == c         = 0
  |a > media && b > media ||
   a > media && c > media ||
   b > media && c > media   = 2
  |otherwise                = 1
    where 
      media = (a+b+c)/3
-- 04.Defina uma função que calcule a mediana de três números. A mediana é o número que fica no meio quando os números são ordenados.
mediana3 :: Float -> Float -> Float -> Float
mediana3 a b c 
  |a >= b && c >= a || c <= a && b >= a = a
  |b >= a && c >= b || c <= b && a >= b = b
  |otherwise                            = c
-- 05.Defina uma função que receba os coeficientes a, b e c de uma equação de segundo grau ax2 + bx + c = 0 e que retorne o número de raízes da equação. Assuma que a é diferente de zero.
qntRaizes :: Int -> Int -> Int -> Int
qntRaizes a b c
  |delta == 0 = 1
  |delta > 0  = 2
  |otherwise  = 0
    where delta = b^2-4*a*c 
-- 06.Defina uma função que receba os coeficientes a, b e c de uma equação de segundo grau ax 2 + bx + c = 0 e que retorne o número de raízes da equação. Qualquer coeficiente pode ser zero. Se a equação tiver infinitas soluções, a função deve retornar 3.
numRaizes :: Int -> Int -> Int -> Int
numRaizes a b c
  |a == 0 && b == 0 && c == 0                               = 3 -- Soluções ∞
  |a == 0 && b == 0 || b == 0 && c == 0 || c == 0 && a == 0 = 0 -- Sem soluções
  |a == 0 && b /= 0 || b == 0 && c /= 0 || c == 0 && a /= 0 = 1 
  |a /= 0 && delta > 0                                      = 2 
  |delta == 0 = 1
  |otherwise  = 0
    where delta = b^2-4*a*c
-- 07.A Locadora de Veículos Eudora lançou uma grande promoção esse mês: pagando apenas R$ 90 por diária, o cliente pode alugar um carro de passeio. Para cada diária, o cliente recebe uma cota de quilometragem de 100 Km. Cada quilômetro a mais custará uma taxa extra de R$ 12. Escreva uma função que receba como entrada a quantidade de dias e a quilometragem total rodada por um cliente dessa locadora e retorne o valor total a ser pago.
valorTotal :: Int -> Int -> Int
valorTotal dia km = dia*90+taxa
 where
   taxa =  if km>100 
     then (km-100)*12 
     else 0
-- 08. Chama-se ano bissexto o ano ao qual é acrescentado um dia extra, ficando ele com 366 dias, um dia a mais do que os anos normais de 365 dias, ocorrendo a cada quatro anos, exceto anos múltiplos de 100 que não são múltiplos de 400. Defina uma função que, dado um ano, indique se este é bissexto.  
anoBissexto :: Int -> String
anoBissexto ano
    |ano `mod` 400 == 0 = "Bissexto"
    |ano `mod` 4 == 0   = "Bissexto"
    |otherwise          = "Nao bissexto"
-- 09. Uma corretora de seguros cobra mais barato se o principal condutor do veículo é mulher e se tem mais de 40 anos. Caso contrário, o valor do seguro fica caro. Defina uma função que receba um valor booleano que indica se o condutor é homem ou mulher (True ou False, respectivamente), outro valor inteiro com a idade em anos do condutor e que retorna um booleano para indicar se o seguro vai ficar barato ou caro (True ou False), respectivamente.
valorSeguro :: Bool -> Int -> String
valorSeguro genero idade  -- Sendo mulher: "True" e homem: "False"
    |genero == True && idade >= 40 = "Barato"
    |otherwise                     = "Caro"

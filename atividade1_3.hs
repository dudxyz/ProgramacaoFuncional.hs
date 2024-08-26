main = return ()
-- 01.Escreva uma função que determine se um inteiro é ímpar, par, positivo, negativo ou nulo. A saída deve ser um String. Um exemplo de saída é “Ímpar positivo”. Defina ao menos duas versões para esta função.
-- Primeira versão.
numDet :: Int -> String
numDet x 
  |x == 0 = "Nulo"
  |x > 0 && even x = "Par e positivo"
  |x < 0 && even x = "Par e negativo"
  |x > 0 && odd x  = "Ímpar e positivo"
  |otherwise       = "Ímpar e Negativo"
--Anualmente, o MEC avalia os cursos universitários de todo o país e atribui conceitos combase em diversos critérios, sendo um deles a quantidade de livros disponíveis. Considerando as regras definidas abaixo, escreva uma função que receba como entrada a quantidade de livros e a quantidade de alunos de um curso, ambos inteiros, e retorne a letra maiúscula correspondente ao conceito concedido pelo MEC.
conceitomec :: Int -> Int -> Char
conceitomec livro aluno
  | alunoporlivro <= 8  ='A'
  | alunoporlivro <= 12 ='B'
  | alunoporlivro <= 18 ='C'
  | alunoporlivro > 18  ='D'
  where
    alunoporlivro = div aluno livro

--Escreva uma função para classificar um triângulo em: escaleno (os três lados de comprimentos diferentes), isósceles (dois lados de comprimentos iguais) ou equilátero (os três lados de comprimentos iguais). A função receberá como dados os tamanhos dos três lados. A função também deverá estabelecer se os três lados efetivamente formam um triângulo.
triangulo :: Int -> Int -> Int -> String
triangulo a b c
  | a >= b+c || b >= a+c || c >= a+b = "Não é Triângulo"
  | a == b && b == c = "Triângulo Equilatero"
  | a == b && b /= c = "Triângulo Isosceles"
  | otherwise = "Triângulo Escaleno"

  --Rafinha sabe que em sua cidade o valor da KWh de energia varia da forma mostrada abaixo. Até 99 KWh: R$1.35 100 até 299 KWh: R$1.55 300 até 574 KWh: R$1.75 Maior ou igual a 575 KWh: R$2.15 Ele também sabe que quando o consumo é maior que 300KWh é cobrado uma taxa de 10% no valor da conta e o preço mínimo de qualquer conta é R$35. Escreva uma função para auxiliar Rafinha a calcular o valor de sua conta elétrica.
valorconta :: Float -> Float
valorconta consumo
  | consumo <= 99  
  || consumo <= 299 
  || consumo > 300 = valorKWh consumo 
  + taxaExtra (valorKWh consumo)
  | otherwise = if valorKWh consumo > 35 then valorKWh consumo else 35
  where
    valorKWh consumo
      | consumo < 100 = 1.35 * consumo
      | consumo < 300 = 1.55 * consumo
      | consumo < 575 = 1.75 * consumo
      |otherwise      = 2.15 * consumo
    taxaExtra valor
      | consumo * 0.1 > 35 =  consumo * 0.1
      | otherwise = 35

main = return ()

type Pixel = Char -- só dois valores válidos: ' ' e '#'

type Pic = [[Pixel]]

mulherPic =
  [ "   ###   ",
    "   ###   ",
    "#   #   #",
    " # ### # ",
    "  #####  ",
    " ####### ",
    "#########",
    "   # #   ",
    "  ## ##  "
  ]

-- Refaça a lista de exercícios sobre imagens bitmaps – do tipo Pic. Porém, desta vez, explore funções de alta ordem: parametrização parcial, seções, composição de funções e expressões lambda. Tente não usar recursão nem compreensões recorrendo a funções pré-definidas tais como map, filter, foldr, zipWith, etc.

-- 1.
showPic :: Pic -> String
showPic pic = concatMap (\n -> n ++ "\n") pic

-- 2.
inverte :: Pic -> String
inverte pic = showPic $ mudaChar pic

mudaChar :: Pic -> Pic
mudaChar pic = map (map inverteChar) pic
  where
    inverteChar '#' = ' '
    inverteChar ' ' = '#'

-- 5. Considerando a definição de árvore binária de busca vista em aula, defina funções para:

{- a. Calcular a profundidade da árvore – isto é, o comprimento do maior caminho
desde a raiz até uma folha.
 b. Calcular o menor elemento da árvore
 c. Calcular o maior elemento
 d. Somar todos os elementos da árvore
 e. Dada uma lista ordenada, transformar a lista em uma árvore binária de busca
 balanceada.
 f. Desafio opcional: Eliminar um dado elemento da árvore. Se o elemento não
 está na árvore, a árvore é inalterada. -}

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Eq, Show, Read, Ord)

arv =
  Node
    0
    ( Node
        5
        ( Node
            8
            (Node 9 Empty Empty)
            (Node 6 Empty Empty)
        )
        (Node 2 (Node 3 Empty Empty) Empty)
    )
    ( Node
        (-6)
        ( Node
            (-3)
            (Node (-2) (Node (-1) Empty Empty) Empty)
            (Node (-5) (Node (-4) Empty Empty) Empty)
        )
        ( Node
            (-9)
            (Node (-8) (Node (-7) Empty Empty) Empty)
            (Node (-10) Empty Empty)
        )
    )
--a.
profundidade Empty = 0
profundidade (Node x left right) = 1 + max (profundidade left) (profundidade right)

--b.
menorElemento Empty = 0
menorElemento (Node x Empty right) = x
menorElemento (Node x left Empty) = min x (menorElemento left)
menorElemento (Node x left right) = min (menorElemento left) (menorElemento right)

--c.
maiorElemento Empty = 0
maiorElemento (Node x left Empty) = x
maiorElemento (Node x Empty right) = max x (maiorElemento right)
maiorElemento (Node x left right) = max (maiorElemento right) (maiorElemento left)

--d.
somaTudo Empty = 0
somaTudo (Node x left right) = x + (somaTudo left) + (somaTudo right)

--e.
listapArv Empty = []
listapArv (Node x Empty Empty) = [x]
listapArv (Node x left Empty) = listapArv left ++ [x]
listapArv (Node x Empty right) = x : listapArv right
listapArv (Node x left right) = listapArv left ++ [x] ++ listapArv right

import Data.Char
main = return ()
--1. Defina funções para:
-- ● Eliminar espaços no início do texto. Consideramos que um caractere é um espaço
--quando satisfaz a propriedade isSpace do módulo Data.Char.
pulaDemarcadores :: String -> String
pulaDemarcadores xs = dropWhile isSpace xs

-- ● Coletar a primeira palavra do texto. Você deve supor que a palavra é composta
--somente por caracteres que não são espaços.
-- primPalavra :: String -> String

-- ● Descartar a primeira palavra do texto.
--descPrimPal :: -> String -> String
-- ● Coletar todas as palavras do texto, gerando uma lista de palavras. Use recursão na
--definição desta definição.
--listaPalavras :: -> String -> [String]
type Doc = String
type Linha = String
type Palavra = String

criaIndice :: Doc -> [([Int], Palavra)]
criaIndice dc = filtraPalavras . fragmentaLinha . enumeraLinhas . divideLinhas $ dc

divideLinhas :: Doc -> [Linha]
divideLinhas = lines

enumeraLinhas :: [Linha] -> [(Int, Linha)]
enumeraLinhas = zip [1..]

separaPalavras :: [(Int, Linha)] -> [(Int, [Palavra])]
separaPalavras linhasNumeradas = [(n, words linha) | (n, linha) <- linhasNumeradas]

fragmentaLinha :: [(Int, [Palavra])] -> [(Int, Palavra)]
fragmentaLinha linhasComPalavras = [(n, palavra) | (n, palavras) <- linhasComPalavras, palavra <- palavras]

filtraPalavras :: [(Int, Palavra)] -> [([Int], Palavra)]
filtraPalavras = filter (\(_, palavra) -> length palavra >= 4)

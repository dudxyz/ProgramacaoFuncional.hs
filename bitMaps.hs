main = return ()
type Pixel = Char -- só dois valores válidos: ' ' e '#'
type Pic = [[Pixel]]
mulherPic = ["   ###   ",
             "   ###   ",
             "#   #   #",
             " # ### # ",
             "  #####  ",
             " ####### ",
             "#########",
             "   # #   ",
             "  ## ##  "
           ]  

mulherPic2 = ["   # #   ",
              "  #####  ",
              "  #####  ",
              "  #   #  ",
              "  #   #  ",
              "  #####  ",
              "  #####  ",
              "   # #   ",
              "   # #   "]
-- 1. 
showPic :: Pic -> String
showPic pic = unlines pic

--2.
inverteBits :: Pic -> Pic
inverteBits = map (map invertePixel)
  where
    invertePixel ' ' = '#'
    invertePixel '#' = ' '

--3. 
flipH :: Pic -> String 
flipH pic = showPic $ map reverse pic

--4. 
flipV :: Pic -> String
flipV pic = concatMap (\x -> x ++ "\n") $ reverse pic

--5. 
sideBySide :: Pic -> Pic -> String
sideBySide pic1 pic2 = showPic $ zipWith (++) pic1 pic2 

--6. 
above :: Pic -> Pic -> String
above pic1 pic2 = showPic $ pic1 ++ pic2

--7. 
superImpose :: Pic -> Pic -> String
superImpose pic1 pic2 = showPic $ zipWith combine pic1 pic2

combine :: [Pixel] -> [Pixel] -> [Pixel]
combine p1 p2 = zipWith combinePixels p1 p2

combinePixels :: Pixel -> Pixel -> Pixel
combinePixels ' ' ' ' = ' ' 
combinePixels _ _ = '#'   

--8.
--scale :: Int -> Pic -> String
--scale n pic = showPic (junta pic)

--junta :: Pic -> Pic
--junta pic = concatMap (concatMap aumenta) pic

--aumenta :: Pixel -> Pic
--aumenta '#' = ["##", "##"]
--aumenta ' ' = ["  ", "  "]

--9. 
-- Função para estender uma figura à direita com pixels brancos
extendRightWith :: Char -> Int -> Pic -> Pic
extendRightWith char n pic = zipWith (++) pic (replicate (length pic) (replicate n char))

-- Função para estender uma figura à esquerda com pixels brancos
extendLeftWith :: Char -> Int -> Pic -> Pic
extendLeftWith fillChar n pic = zipWith (++) (replicate (length pic) (replicate n fillChar)) pic

-- Função para estender uma figura acima com pixels brancos
extendAboveWith :: Char -> Int -> Pic -> Pic
extendAboveWith fillChar n pic = replicate n (replicate (length (head pic)) fillChar) ++ pic

-- Função para estender uma figura abaixo com pixels brancos
extendBelowWith :: Char -> Int -> Pic -> Pic
extendBelowWith fillChar n pic = pic ++ replicate n (replicate (length (head pic)) fillChar)

-- Função para estender horizontalmente com pixels brancos (centralizado)
extendHorizontallyWith :: Char -> Int -> Pic -> Pic
extendHorizontallyWith fillChar n pic =
    let originalWidth = length (head pic)
        leftPadding = (n - 1) `div` 2
        rightPadding = n - leftPadding
        leftExtension = replicate leftPadding (replicate originalWidth fillChar)
        rightExtension = replicate rightPadding (replicate originalWidth fillChar)
    in zipWith (++) (zipWith (++) leftExtension pic) rightExtension

-- Função para estender verticalmente com pixels brancos (centralizado)
extendVerticallyWith :: Char -> Int -> Pic -> Pic
extendVerticallyWith fillChar n pic =
    let originalHeight = length pic
        topPadding = (n - 1) `div` 2
        bottomPadding = n - topPadding
        topExtension = replicate topPadding (replicate (length (head pic)) fillChar)
        bottomExtension = replicate bottomPadding (replicate (length (head pic)) fillChar)
    in topExtension ++ pic ++ bottomExtension

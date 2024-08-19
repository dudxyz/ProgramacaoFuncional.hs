{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CodeWorld
import Data.Char
import System.Random

main = do
       g <- newStdGen
       let rnds = randomRs (1, 10) g :: [Int]
       activityOf World { matrizAliens     = (matrizAliens1, matrizAliens2),
                          posMatrizAliens  = (0, 0),
                          dirAlien         = 1, 
                          tmrAlien         = 0.5, 
                          posXNave         = 0, 
                          dirNave          = 0,
                          posBala          = (0, -11), 
                          velXBala         = 0, 
                          movBala          = 0,
                          rndms            = rnds,
                          tmrProjAliens    = 0.3,
                          projeteisAliens  = [],
                          tmrExplosao      = 1,
                          animaExplosao    = (explo1Pic, explo2Pic),
                          tmrAnimaExplosao = 0.1,
                          tudoPara         = 1,
                          explodindo       = False,
                          vidas            = [((4, 9), navePic), 
                                              ((6, 9), navePic), 
                                              ((8, 9), navePic)],
                          morteAlien       = [],
                          tmrMorteAlien    = 0.1,
                          alienMorrendo    = False }
                  atualize vizualize

type Pixel = Char
type Pic   = [[Pixel]]

data World = World { matrizAliens :: ([Alien], [Alien]),
                     posMatrizAliens :: Point,
                     dirAlien :: Double,
                     tmrAlien :: Double,
                     posXNave :: Double,
                     dirNave :: Double,
                     posBala :: Point,
                     velXBala :: Double,
                     movBala :: Double,
                     tmrProjAliens :: Double,
                     projeteisAliens :: [ProjetilAlien],
                     rndms :: [Int],
                     tmrExplosao :: Double,
                     animaExplosao :: (Pic, Pic),
                     tmrAnimaExplosao :: Double,
                     vidas :: [Vida],
                     morteAlien :: [ExplodeAlien],
                     tmrMorteAlien :: Double,
                     alienMorrendo :: Bool,
                     tudoPara :: Double,
                     explodindo :: Bool}
                     

type Alien = (Point, Pic)
type ProjetilAlien = Point 
type Vida = (Point, Pic)
type ExplodeAlien = Point

matrizAliens1 = zip pontos listaAliens1
matrizAliens2 = zip pontos listaAliens2

pontos = zip (concat . replicate 5 $ [-5..5]) ((replicate 11 6) ++ 
                                               (replicate 11 5) ++
                                               (replicate 11 4) ++
                                               (replicate 11 3) ++
                                               (replicate 11 2)) 
                                               
listaAliens1 = (replicate 11 alien3_1) ++ (replicate 22 alien2_1) ++ (replicate 22 alien1_1)
listaAliens2 = (replicate 11 alien3_2) ++ (replicate 22 alien2_2) ++ (replicate 22 alien1_2)


alien1_1 = [ "     ####     ",
             " ############ ",
             "##############",
             "####  ##  ####",
             "##############",
             "   ###  ###   ",
             "  ##  ##  ##  ",
             "   ##    ##   " ]
             
alien1_2 = [ "     ####     ",
             " ############ ",
             "##############",
             "####  ##  ####",
             "##############",
             "   ###  ###   ",
             "  ##  ##  ##  ",
             "##          ##" ]
             
alien2_1 = [ "  #     #  ",
             "   #   #   ",
             "  #######  ",
             "### ### ###",
             "###########",
             "# ####### #",
             "# #     # #",
             "   ## ##   " ]
             
alien2_2 = [ "  #     #  ",
             "#  #   #  #",
             "# ####### #",
             "### ### ###",
             "###########",
             "  #######  ",
             "  #     #  ",
             " #       # " ]
             
alien3_1 = [ "   ##   ",
             "  ####  ",
             " ###### ",
             "## ## ##",
             "########",
             " # ## # ",
             "#      #",
             " #    # " ]
             
alien3_2 = [ "   ##   ",
             "  ####  ",
             " ###### ",
             "## ## ##",
             "########",
             "  #  #  ",
             " # ## # ",
             "# #  # #" ]
                     
navePic = [ "       #       ",
            "      ###      ",
            "      ###      ",
            " ############# ",
            "###############",
            "###############",
            "###############" ]
            
projetilPic = [ " # ",
                " # ",
                "###",
                " # " ]
                
explo1Pic = [ "      #      # ",
              "         #  #  ",
              "  #   #        ",
              "       ## ##   ",
              "#    # ## # #  ",
              "  #########  # ",
              " ########### # " ]

explo2Pic = [ " #          #  ",
              "  #   ##  ##   ",
              "    #        # ",
              "#   ##  ##    #",
              " #   ###    #  ",
              "  ########     ",
              " ## ######   # " ]
              
morteAlienPic = [ "#   #   #",
                  " #  #  # ", 
                  "  # # #  ", 
                  "         ",
                  "###   ###",
                  "         ",
                  "  # # #  ",
                  " #  #  # ",
                  "#   #   #" ]
             
picToPicture :: Pic -> Picture
picToPicture pic = translated ((-larguraPic/2)+1) ((-alturaPic/2)+1) .
                   pictures .
                   map (\((x, y), p) -> translated x y $ solidRectangle 1 1) .
                   filter (\(_, p) -> p == '#') .
                   map (\(x, (y, p)) -> ((x, y), p)) .
                   zip (concat . (replicate . length $ head pic) $ [0 .. (larguraPic - 1)]) . 
                   concatMap distribui . 
                   zip [0..] $ 
                  (reverse pic)
    where
        distribui (n, xs) = map (\x -> (n, x)) xs
        larguraPic        = fromIntegral . length . head $ pic
        alturaPic         = fromIntegral . length $ pic
                                 
                 
posYNave   = -8
deslAliens = 0.2
velXNave   = 5
velYBala   = 8
velYProj   = -6

vizualize :: World -> Picture
vizualize w@World {..} = vizualizeAliens w & 
                         vizualizeNaveOuExplosao w & 
                         vizualizeBala w & 
                         vizualizeProjeteis w &
                         vizualizeVidas w &
                         vizualizeMorteAlien w &
                         background
    where 
        background = solidRectangle 20 20

vizualizeAliens World {..} = colored white .
                             translated ax ay .
                             dilated 1.2 .
                             pictures . 
                             map transformaPic $
                             ma1
     where
         (ma1, ma2)                  = matrizAliens
         (ax, ay)                    = posMatrizAliens
         transformaPic ((x, y), pic) = translated x y (dilated 0.06 (picToPicture pic))
         
vizualizeNaveOuExplosao World {..}
                        | tudoPara == 0 = colored green . translated posXNave posYNave . dilated 0.1 . picToPicture $ exp1
                        | otherwise     = colored green . translated posXNave posYNave . dilated 0.1 . picToPicture $ navePic
    where 
        (exp1, exp2) = animaExplosao

vizualizeBala World {posBala = (bx, by)} = colored white . translated bx by $ solidRectangle 0.15 0.3

vizualizeProjeteis World {..} = pictures . map (\(x, y) -> colored white . translated x y . dilated 0.1 $ picToPicture projetilPic) $ projeteisAliens

vizualizeVidas World {..} = pictures . map (\((vx, vy), p) -> colored green . translated vx vy . dilated 0.1 $ picToPicture p) $ vidas

vizualizeMorteAlien World {..} 
                   | tmrMorteAlien <= 0 = blank
                   | otherwise          = colored white . pictures . map (\(ax, ay) -> translated ax (ay + 0.25) . dilated 0.1 $ picToPicture morteAlienPic) $ morteAlien
   
                    
atualize :: Event -> World -> World 
atualize (TimePassing t) w@World {..} = mortesAliens t .
                                        explosaoNave t .
                                        disparoAliens t .
                                        atiraBala t .
                                        movNave t .
                                        movAliens t $
                                        w
                                  
atualize (KeyPress "Right")   w@World {..} = moveNaveD w
atualize (KeyRelease "Right") w@World {..} = paraNaveD w
atualize (KeyPress "Left")    w@World {..} = moveNaveE w
atualize (KeyRelease "Left")  w@World {..} = paraNaveE w
atualize (KeyPress " ")       w@World {..} = atira w
atualize _ w                               = w
                                                      
movAliens t w@World {..} 
                   | tmrAlien > 0                    = w {tmrAlien = tmrAlien - t}
                   | dirAlien == 1 && newAX > 3.5 ||
                     dirAlien == -1 && newAX < -3.5  = w {matrizAliens    = mudaAliens (ma1, ma2),
                                                          posMatrizAliens = (ax, newAY),
                                                          dirAlien        = -dirAlien,
                                                          tmrAlien        = 0.5}
                   | otherwise                       = w {matrizAliens    = if tudoPara == 0
                                                                            then matrizAliens
                                                                            else mudaAliens (ma1, ma2),
                                                          posMatrizAliens = (newAX, ay),
                                                          tmrAlien        = 0.5}
    where 
        (ma1, ma2)            = matrizAliens
        (ax, ay)              = posMatrizAliens
        mudaAliens (ma1, ma2) = (ma2, ma1)
        newAX                 = ax + dirAlien*deslAliens*tudoPara
        newAY                 = ay - 0.5*tudoPara
        
movNave t w@World {..} = w {posXNave = posXNave + dirNave*velXNave*t*tudoPara}
                                                 
atiraBala t w@World {..} = w {posBala  = (bx + movBala*velXBala*t, by + movBala*velYBala*t)}
    where
        (bx, by) = posBala
        
moveNaveD w@World {..} = w {dirNave  = 1 }
paraNaveD w@World {..}
          | dirNave == -1 = w
          | otherwise = w {dirNave  = 0 }
moveNaveE w@World {..} = w {dirNave  = -1 }
paraNaveE w@World {..} 
     | dirNave == 1  = w
     | otherwise = w {dirNave  = 0 }
                                          
atira w@World {..}
      | bx <= -10 || bx >= 10 || 
        by >= 10  || by <= -10   = w {posBala  = (posXNave, posYNave),
                                      velXBala = dirNave*velXNave,
                                      movBala  = 1 }
      | otherwise                = w {movBala  = 1 }
    where
        (bx, by) = posBala

disparoAliens t w@World {..}
         | tudoPara == 0       = w
         | tmrProjAliens <= 0  = w {tmrProjAliens   = novoTimer,
                                    projeteisAliens = desceProj projeteisAtualizados,
                                    rndms           = restoRnds }
         | otherwise = w {tmrProjAliens   = tmrProjAliens - t,
                          projeteisAliens = desceProj projeteisAliens}
                
   where
       (ma1, ma2) = matrizAliens
       rnd1 : rnd2 : restoRnds = rndms
       projeteisAtualizados 
           | posAtiradores == [] = projeteisAliens
           | otherwise           = novoProjetil : projeteisAliens
       novoProjetil  = posAtiradores !! mod rnd2 (length posAtiradores)
       posAtiradores = map fst $ take 11 posAlienAtual
       novoTimer     = fromIntegral rnd1 * 0.3
       desceProj     = map (\(px, py) -> (px, py + velYProj * t))
       posAlienAtual = map (\((ax, ay), p) -> ((ax + pmx, ay + pmy), p)) ma1
       (pmx, pmy)    = posMatrizAliens
       
       
explosaoNave t w@World {..}
             | projetilAtingeNave           = w {projeteisAliens  = [],
                                                 vidas            = tail vidas,
                                                 tudoPara         = 0,
                                                 explodindo       = True}  
             | tmrExplosao < 0              = w {posXNave         = 0,
                                                 tmrExplosao      = 1,
                                                 tmrAnimaExplosao = 0.1,
                                                 tudoPara         = 1,
                                                 explodindo       = False}
             | explodindo                   = w {tmrExplosao      = tmrExplosao - t,
                                                 animaExplosao    = trocaImagens animaExplosao,
                                                 tmrAnimaExplosao = tmrAnimaExplosao - t}
             | vidas == [] || 
               (posYAliens + 3) == posYNave = w {matrizAliens     = (matrizAliens1, matrizAliens2),
                                                 posMatrizAliens  = (0, 0),
                                                 dirAlien         = 1, 
                                                 posXNave         = 0, 
                                                 tmrExplosao      = 1,
                                                 tmrAnimaExplosao = 0.1,
                                                 tudoPara         = 1,
                                                 explodindo       = False,
                                                 vidas            = [((4, 9), navePic), ((6, 9), navePic), ((8, 9), navePic)] }                            
             | otherwise = w
    where 
        projetilAtingeNave        = any colide projeteisAliens
        colide (px, py)           = dist (px, py) (posXNave, posYNave) <= 0.5
        dist (x1, y1) (x2, y2)    = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        trocaImagens (exp1, exp2) = (exp2, exp1)
        (_, posYAliens)           = posMatrizAliens
                                        
mortesAliens t w@World {..}
             | haColisao                        = w {matrizAliens  = atualizaMatrizAliens,
                                                     posBala       = (0, -11),
                                                     movBala       = 0,
                                                     morteAlien    = (nmx, nmy) : morteAlien,
                                                     alienMorrendo = True }
             | tmrMorteAlien <= 0               = w {morteAlien     = init morteAlien,
                                                     tmrMorteAlien  = 0.1,
                                                     alienMorrendo  = False}
             | alienMorrendo                    = w {tmrMorteAlien  = tmrMorteAlien - t}
             | matrizAliens == ([],[])          = w {matrizAliens   = (matrizAliens1, matrizAliens2),
                                                     posMatrizAliens = (0, 0),
                                                     dirAlien        = 1,
                                                     posXNave        = 0}
             | otherwise = w
    where
        haColisao             = any colide . map fst $ posAlienAtual
        colide posAlien       = dist posAlien posBala <= 0.5
        (ma1, ma2)            = matrizAliens  
        atualizaMatrizAliens  = (filtraAlien ma1, filtraAlien ma2)
        filtraAlien           = filter (\((ax, ay), _) -> (ax, ay) /= (nmx - pmx, nmy - pmy))
        (nmx, nmy)            = posDoAlien $ map fst posAlienAtual
        posDoAlien [posAlien] = posAlien
        posDoAlien (posAlien : ps)
                   | colide posAlien = posAlien
                   | otherwise       = posDoAlien ps  
        dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)  
        posAlienAtual          = map (\((ax, ay), p) -> ((ax + pmx, ay + pmy), p)) ma1
        (pmx, pmy)             = posMatrizAliens

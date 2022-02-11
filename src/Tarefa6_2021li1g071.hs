{- |
Module      : Tarefa6_2021li1g071
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1g071 where

import LI12122
import Tarefa2_2021li1g071
import Tarefa4_2021li1g071

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m2r :: Mapa
m2r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m2r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

doorCoords2 :: [(Peca, Coordenadas)] -> Coordenadas
doorCoords2 ((a,b):r) = if a == Porta then b else doorCoords2 r

doorCoords :: Mapa -> (Int, Int)
doorCoords mapa = doorCoords2 (desconstroiMapa mapa)

checkDoor :: Int -> Jogo -> Int
checkDoor i (Jogo mapa (Jogador (x,y) _ _)) = if (x - i > (fst (doorCoords mapa)))
                                              then 0
                                              else 1 --placeholder

solution :: Jogo -> [Movimento]
solution (Jogo mapa (Jogador (x,y) d b)) = if (doorCoords mapa == (x,y)) then []
                                           else if (x > xs) --personagem encontra-se do lado direito da porta 
                                                then if (locatePiece (x-1, y) mapa == Vazio) || (locatePiece (x-1, y) mapa == Porta)
                                                     then [AndarEsquerda] ++ solution (Jogo mapa (Jogador (checkFallPlace (x-1,y) mapa) d b))
                                                     else if (locatePiece (x-1, y-1) mapa /= Bloco) && (locatePiece (x-1, y-1) mapa /= Caixa)
                                                          then [Trepar] ++ solution (Jogo mapa (Jogador (x-1,y) d b))
                                                          else [] {-if (locatePiece (x-1, y-1) mapa == Bloco) || (b == False) 
                                                               then 
                                                               else-}
                                                else if (locatePiece (x+1, y) mapa == Vazio) || (locatePiece (x+1, y) mapa == Porta) 
                                                     then [AndarEsquerda] ++ solution (Jogo mapa (Jogador (checkFallPlace (x+1,y) mapa) d b))
                                                     else if (locatePiece (x+1, y-1) mapa /= Bloco) && (locatePiece (x+1, y-1) mapa /= Caixa)
                                                          then [Trepar] ++ solution (Jogo mapa (Jogador (x+1,y) d b))
                                                          else [] {-if (locatePiece (x+1, y-1) mapa == Bloco) || (b == False) 
                                                               then 
                                                               else-}
    where ys = (snd (doorCoords mapa))
          xs = (fst (doorCoords mapa))

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo = if (checkDoor i jogo == 0) --testes iniciais
                     then Nothing
                     else Just (solution jogo) --placeholder

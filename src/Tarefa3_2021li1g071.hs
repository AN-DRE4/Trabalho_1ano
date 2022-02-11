{- |
Module      : Tarefa3_2021li1g071
Description : Representação textual do jogo
Copyright   : André Castro Alves <a95033@alunos.uminho.pt>;
            : Nuno Igreja <a97433@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g071 where

import LI12122

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

showListWOP :: [Peca] -> String
showListWOP [] = []
{-showListWOP (h:r) = case h of Vazio -> show " " ++ "" ++ showListWOP r --criar um data com X, C, P e "espaco"
                              Bloco -> show "X" ++ "" ++ showListWOP r
                              Caixa -> show "C" ++ " " ++ showListWOP r
                              Porta -> show "P" ++ "" ++ showListWOP r-}
showListWOP (h:r) = show h ++ "" ++ showListWOP r                              

showListWP :: [Peca] -> Jogador -> String
showListWP [] _ = []
{-showListWP (h:r) (Jogador (x,y) d b) = if x > 0
                                       then case h of Vazio -> show " " ++ "" ++ showListWP r (Jogador (x-1,y) d b)
                                                      Bloco -> show "X" ++ "" ++ showListWP r (Jogador (x-1,y) d b)
                                                      Caixa -> show "C" ++ "" ++ showListWP r (Jogador (x-1,y) d b)
                                                      Porta -> show "P" ++ "" ++ showListWP r (Jogador (x-1,y) d b)
                                       else if d == Este
                                            then show ">" ++ "" ++ showListWOP (h:r)
                                            else show "<" ++ "" ++ showListWOP (h:r)-}
showListWP (h:r) (Jogador (x,y) d b) | x /= 0 = show h ++ "" ++ showListWP r (Jogador (x-1,y) d b)                                          
                                     | otherwise = show (Jogador (x,y) d b) ++ "" ++ showListWOP r
showJogo2 :: Mapa -> String
showJogo2 [] = []
showJogo2 [h] = showListWOP h
showJogo2 (h:r) = showListWOP h ++ "\n" ++ showJogo2 r

showJogo' :: Jogo -> Int -> String
showJogo' (Jogo [] _) _ = []
showJogo' (Jogo [h] (Jogador (x,y) d b)) _ | y == 0 = showListWP h (Jogador (x,y) d b)
                                           | otherwise = showListWOP h
showJogo' (Jogo ((h:t) : r) (Jogador (x,y) d b)) k = if y /= k
                                                     then showListWOP (h:t) ++ "\n" ++ showJogo' (Jogo r (Jogador (x,y) d b)) (k+1)
                                                     else showListWP (h:t) (Jogador (x,y) d b) ++ "\n" ++ showJogo2 r

showJogo :: Jogo -> String
showJogo (Jogo [] _) = []
showJogo j = showJogo' j 0

instance Show Direcao where
  show (Este) = ">"
  show (Oeste) = "<"

instance Show Jogador where
  show (Jogador (x,y) d b) = show d

instance Show Peca where
  show (Bloco) = "X"
  show (Porta) = "P" 
  show (Caixa) = "C" 
  show (Vazio) = " "

instance Show Jogo where
  show = showJogo


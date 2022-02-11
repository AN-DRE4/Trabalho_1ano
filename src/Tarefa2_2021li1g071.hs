{- |
Module      : Tarefa2_2021li1g071
Description : Construção/Desconstrução do mapa
Copyright   : André Castro Alves <a95033@alunos.uminho.pt>;
            : Nuno Igreja <a97433@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g071 where

import Tarefa3_2021li1g071 
import Data.List
import LI12122

estaOrganizado :: [(Peca, Coordenadas)] -> Bool
estaOrganizado [] = True
estaOrganizado [x] = True
estaOrganizado [(_, (a,b)) , (_, (as, bs))] | b > bs = False
                                            | (b == bs) && (a > as) = False
                                            | otherwise = True
estaOrganizado ((_, (a,b)) : (p, (as,bs)) : r) | b > bs = False
                                               | (b == bs) && (a > as) = False
                                               | otherwise = estaOrganizado ((p, (as,bs)) : r)

organizar :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
organizar [] = []
organizar [(p, (a,b)) , (ps, (as,bs))] | b > bs = [(ps, (as,bs)) , (p, (a,b))]
                                       | (b == bs) && (a > as) =  [(ps, (as,bs)) , (p, (a,b))]
                                       | otherwise = [(p, (a,b)) , (ps, (as,bs))]
organizar ((p, (a,b)) : (ps, (as,bs)) : r) | b > bs = organizar((ps, (as,bs)) : (p, (a,b)) : r)
                                           | (b == bs) && (a > as) = ((ps, (as,bs)) : (p, (a,b)) : r)
                                           | otherwise = (p, (a,b)) : (organizar ((ps, (as,bs)) : r))

biggestY :: [(Peca, Coordenadas)] -> Int 
biggestY [] = -1
biggestY [(_, (a,b))] = b
biggestY ((_,(x,y)):r) = if y > biggestY r
                         then y
                         else biggestY r

biggestX :: [(Peca, Coordenadas)] -> Int 
biggestX [] = -1
biggestX [(_, (a,b))] = a
biggestX ((_,(x,y)):r) = if x > biggestX r
                         then x
                         else biggestX r

geraLinhaVazia :: Int -> [Peca]
geraLinhaVazia 0 = []
geraLinhaVazia y | y > 0 = Vazio : geraLinhaVazia (y-1)
                 | otherwise = []

sby :: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]]
sby [] = []
sby (h@(_,(_,hy)):t) = (h : it) : ot
 where
  (it, ot) = case sby t of
    [] -> ([], [])
    t'@(((_, (_, ny)):_):_) | hy < ny -> ([], t')
    ht:tt -> (ht, tt)

convertMapa :: [(Peca,Coordenadas)] -> Int -> Int -> [Peca]
convertMapa [] a x = if a >= x
                     then []
                     else Vazio : convertMapa [] (a+1) x
convertMapa ((p,(a,b)): r) x y | a == x = p : convertMapa r (x+1) y
                               | otherwise = Vazio : convertMapa ((p,(a,b)): r) (x+1) y

convertLMapa :: [[(Peca,Coordenadas)]] -> Int -> Int -> Int -> Mapa
convertLMapa [[]] a x c = if (x >= a) || (x > c)
                          then [[]]
                          else geraLinhaVazia (a+1) : convertLMapa [[]] a (x+1) c
convertLMapa [((p,(a,b)):r)] x y c | b > y = geraLinhaVazia (x+1) : convertLMapa [((p,(a,b)):r)] x (y+1) c
                                   | b == c = [convertMapa ((p,(a,b)):r) 0 x]
                                   | otherwise = convertMapa ((p,(a,b)):r) 0 x : convertLMapa [[]] x (y+1) c                          
convertLMapa (((p,(a,b)):r):t) x y c | b > y = geraLinhaVazia (x+1) : convertLMapa (((p,(a,b)):r):t) x (y+1) c
                                     | otherwise = convertMapa ((p,(a,b)):r) 0 x : convertLMapa t x (y+1) c

constroiMapa7 :: [(Peca, Coordenadas)] -> Mapa
constroiMapa7 [] = [[]]
constroiMapa7 ((p,(a,b)):r) = convertLMapa (sby ((p,(a,b)):r)) bigX 0 bigY
      where pec = ((p,(a,b)):r) 
            bigX = biggestX pec
            bigY = biggestY pec

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas | estaOrganizado pecas == False = constroiMapa (organizar pecas)
                   | otherwise = constroiMapa7 pecas
      where x = biggestX pecas                                       

cleanUpMap :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
cleanUpMap [] = []
cleanUpMap ((p,c):r) | p == Vazio = cleanUpMap r
                     | otherwise = (p,c) : cleanUpMap r

desconstroiMapa2 :: Mapa -> Coordenadas -> [(Peca, Coordenadas)]
desconstroiMapa2 [] _ = []
desconstroiMapa2 ([]: r) (a,b) = desconstroiMapa2 r (0, b+1)
desconstroiMapa2 ((p:o):r) (a,b) = case p of Bloco -> (Bloco, (a,b)) : desconstroiMapa2 ((o):r) (a+1, b)
                                             Porta -> (Porta, (a,b)) : desconstroiMapa2 ((o):r) (a+1, b)
                                             Caixa -> (Caixa, (a,b)) : desconstroiMapa2 ((o):r) (a+1, b)
                                             Vazio -> (Vazio, (a,b)) : desconstroiMapa2 ((o):r) (a+1, b)

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa l = cleanUpMap (desconstroiMapa2 l (0,0))

{- |
Module      : Tarefa4_2021li1g071
Description : Movimentação do personagem
Copyright   : André Castro Alves <a95033@alunos.uminho.pt>;
            : Nuno Igreja <a97433@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g071 where

import Tarefa3_2021li1g071
import LI12122

{- | A função 'locatePiece' vai retornar a peça que se encontra na posição dada pelo par de coordenadas recebido pela função

== Exemplos de utilização:
>>> locatePiece (0,1) [[Vazio, Bloco], [Caixa, Porta]]
Caixa
>>> locatePiece (1,0) [[Vazio, Bloco], [Caixa, Porta]]
Bloco

-}

locatePiece :: Coordenadas -> Mapa -> Peca
locatePiece (x,y) ((h:t) : r) | y > 0 = locatePiece (x,y-1) r
                              | x > 0 = locatePiece (x-1, y) (t : r)
                              | otherwise = h

{- | A função 'checkFallPlace' em combinação com a 'locatePiece' vai retornar a posição final de um buraco para uma queda de um Player/Caixa

== Exemplos de utilização:
>>> checkFallPlace (1,0) [[Bloco, Vazio, Bloco], [Bloco, Bloco, Bloco], [Bloco, Bloco, Bloco], [Bloco, Bloco, Bloco], [Bloco, Bloco, Bloco]]
(1,0)
Caixa
>>> checkFallPlace (1,0) [[Bloco, Vazio, Bloco], [Bloco, Vazio, Bloco], [Bloco, Vazio, Bloco], [Bloco, Vazio, Bloco], [Bloco, Bloco, Bloco]]
(1,3)

-}                              

checkFallPlace :: Coordenadas -> Mapa -> Coordenadas
checkFallPlace (x,y) m = if (locatePiece (x, y+1) m == Bloco) || (locatePiece (x, y+1) m == Caixa)
                         then (x,y)
                         else checkFallPlace (x, y+1) m

{- | A função 'changePiece2' vai modificar a peça que se encontra na posição pretendida de uma lista de peças para a peça que nos foi dada

== Exemplos de utilização:
>>> changePiece2 [Bloco, Vazio, Bloco] (2,0) Caixa
[Bloco, Vazio, Caixa]
>>> changePiece2 [Bloco, Vazio, Bloco] (1,1) Caixa
[Bloco, Caixa, Bloco]

-}
                    
changePiece2 :: [Peca] -> Coordenadas -> Peca -> [Peca]
changePiece2 (h:t) (x,y) p | x > 0 = h : changePiece2 t (x-1,y) p
                           | otherwise = p:t

{- | A função 'changePiece1' em combinação com a 'changePiece2' vai modificar a peça que se encontra na posição pretendida do mapa para a peça que nos foi dada

== Exemplos de utilização:
>>> changePiece1 [[Bloco, Vazio, Bloco], [Bloco, Bloco, Bloco]] (2,0) Caixa
[[Bloco, Vazio, Caixa], [Bloco, Bloco, Bloco]]
>>> changePiece1 [[Bloco, Vazio, Bloco], [Bloco, Bloco, Bloco]] (1,1) Vazio
[[Bloco, Vazio, Bloco], [Bloco, Vazio, Bloco]]

-}                            

changePiece1 :: Mapa -> Coordenadas -> Peca -> Mapa
changePiece1 ((h:t):r) (x,y) p | y > 0 = (h:t) : changePiece1 r (x,y-1) p
                               | x > 0 = (h : changePiece2 t (x-1, y) p) : r
                               | otherwise = (p:t) : r

{- | A função 'biggestMapX' vai devolver o comprimento do mapa

== Exemplos de utilização:
>>> biggestMapX [[Bloco, Vazio, Bloco], [Bloco, Bloco, Bloco]]
3
>>> biggestMapX [[Bloco, Vazio], [Bloco, Bloco]]
2

-}                          

biggestMapX :: Jogo -> Int
biggestMapX (Jogo ((h:t):r) _) = length(h:t) 

{- | A função 'moveJogador' vai ser a função mais importante desta Tarefa, sendo que será nesta que se executará os movimentos dos jogadores. Estes 4 movimentos são:

* AndarEsquerda - Faz com que o jogador ande uma unidade para a sua esquerda, caso a peça à sua esquerda seja ou Vazio ou Porta, onde independente da sua direção 
(Oeste ou Este) terá sua direção mudada para Oeste. Para além disso se o jogador carregar uma caixa, ele apenas irá se mover se a peça uma unidade acima da que se 
encontra à sua esquerda também for Vazio. Caso exista um buraco no lugar onde o jogador se iria mover, o jogador irá se mover para o lugar final da queda. Caso 
o jogador se encontre na borda esquerda do mapa, ele não consegue se mover mais para a esquerda, logo não se movimentará   

* AndarDireita - Terá a mesma função que a AndarEsquerda, porém de forma espelhada, realizando tudo para a direita do jogador e não para a esquerda, mudando também 
a sua direção para Este. Também como na AndarEsquerda, caso o jogador se encontre na borda direita do mapa, ele não consegue se mover mais para a direita, 
logo não se movimentará   

* Trepar - Caso o jogador receba este movimento, se a peça que se encontre a uma unidade dele em relação à sua direção for bloco ou caixa o jogador trepará essa 
peça, ou seja, a localização do jogador se tornará a localização da peça em cima do bloco que se encontrava à sua frente na sua direção. Caso o jogador carregue 
uma caixa, teremos de verificar se a peça duas posições a cima do bloco em frente ao jogador é também Vazio     

* InterageCaixa - Neste caso irá haver duas situações distintas:
     
     1. Caso o jogador carregue uma caixa - neste caso o jogador irá largar a caixa que possui na posição diretamente à sua frente consoante a sua direção, onde 
     esta poderá ser largada também em cima de um bloco ou caixa se a peça que se encontre em frente ao jogador for um desses dois e a que se encontre em cima 
     dela for um Vazio. Caso exista um buraco em frente ao jogador, a caixa irá para o último lugar dessa coluna válida para a caixa. No final disto se ele 
     conseguir largar a caixa com sucesso o seu parâmetro relacionada com carregar a caixa tornar-se-á False

     2. Caso ele não carregue uma - neste caso o jogador irá tentar carregar uma caixa, logo vai ver se a peça em frente a ele é uma caixa, onde se isso for
     verdade, caso a peça a cima do jogador e em cima da caixa for Vazio, o parâmetro relacionada com carregar a caixa tornar-se-á True e o local onde se encontrava
     a caixa virará Vazio

== Funções auxiliares utilizadas:

* 'locatePiece'

* 'checkFallPlace'

* 'biggestMapX'

* 'changePiece1'

== Notas:

Ao criar as implementaçôes relacionadas com caixas, inicialmene assumi que quando um jogador carrega-se uma caixa, essa era carregada no topo dele, ou seja o bloco 
acima do jogador seria sempre uma caixa, logo quando este se move-se a caixa que estaria supostamente no topo dele se moveria junto, porém ao correr os testes 
disponibilizados pelos professores deparei-me que quando um jogador carregava uma caixa esta não era colocada no mapa em cima do jogador, mas sim eliminada do mapa, 
ou seja, quando um jogador que não carrega-se uma caixa interagi-se com uma, o lugar dela seria trocado por vazio e o parâmetro do jogador que indica se carrega 
uma caixa torna-se True, e o contrário também acontece, quando um jogador que carrega uma caixa tenta descarrega-la, apenas irá trocar a peça suposta por Caixa e 
o parâmetro do jogador muda por False   

-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo ((h:t) : r) (Jogador (x,y) d b)) movimento = case movimento of 
    AndarEsquerda -> if x == 0 
                     then (Jogo ((h:t) : r) (Jogador (x,y) Oeste b))
                     else if ((locatePiece (x-1,y) ((h:t) : r) == Vazio) || (locatePiece (x-1,y) ((h:t) : r) == Porta)) && (locatePiece (x-1,y+1) ((h:t) : r) /= Vazio)
                          then if b == False
                               then (Jogo ((h:t) : r) (Jogador (x-1,y) Oeste b))
                               else if locatePiece (x-1, y-1) ((h:t) : r) == Vazio
                                    then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x-1,y-1) Caixa) (x,y-1) Vazio) (Jogador (x-1,y) Oeste b))
                                    else (Jogo ((h:t) : r) (Jogador (x,y) Oeste b))
                          else if ((locatePiece (x-1,y) ((h:t) : r) == Vazio) || (locatePiece (x-1,y) ((h:t) : r) == Porta)) && (locatePiece (x-1,y+1) ((h:t) : r) == Vazio)
                               then if b == True
                                    then if locatePiece (x-1, y-1) ((h:t) : r) == Vazio
                                         then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x-1, snd (checkFallPlace (x-1, y+1) ((h:t) : r)) - 1) Caixa) (x,y-1) Vazio) (Jogador (checkFallPlace (x-1, y+1) ((h:t) : r)) Oeste b))
                                         else (Jogo ((h:t) : r) (Jogador (x,y) Oeste b))
                                    else (Jogo ((h:t) : r) (Jogador (checkFallPlace (x-1, y+1) ((h:t) : r)) Oeste b))
                               else (Jogo ((h:t) : r) (Jogador (x,y) Oeste b))
    AndarDireita -> if x == (biggestMapX (Jogo ((h:t) : r) (Jogador (x,y) d b)) - 1)
                    then (Jogo ((h:t) : r) (Jogador (x,y) Este b))
                     else if ((locatePiece (x+1,y) ((h:t) : r) == Vazio) || (locatePiece (x+1,y) ((h:t) : r) == Porta)) && (locatePiece (x+1,y+1) ((h:t) : r) /= Vazio)
                          then if b == False
                               then (Jogo ((h:t) : r) (Jogador (x+1,y) Este b))
                               else if locatePiece (x+1, y-1) ((h:t) : r) == Vazio
                                    then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x+1,y-1) Caixa) (x,y-1) Vazio) (Jogador (x+1,y) Este b))
                                    else (Jogo ((h:t) : r) (Jogador (x,y) Este b))
                          else if ((locatePiece (x+1,y) ((h:t) : r) == Vazio) || (locatePiece (x+1,y) ((h:t) : r) == Porta)) && (locatePiece (x+1,y+1) ((h:t) : r) == Vazio)
                               then if b == True
                                    then if locatePiece (x+1, y-1) ((h:t) : r) == Vazio
                                         then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x+1, snd (checkFallPlace (x+1, y+1) ((h:t) : r)) - 1) Caixa) (x,y-1) Vazio) (Jogador (checkFallPlace (x+1, y+1) ((h:t) : r)) Este b))
                                         else (Jogo ((h:t) : r) (Jogador (x,y) Este b))
                                    else (Jogo ((h:t) : r) (Jogador (checkFallPlace (x+1, y+1) ((h:t) : r)) Este b))
                               else (Jogo ((h:t) : r) (Jogador (x,y) Este b))
    Trepar -> case d of
        Oeste -> if (locatePiece (x-1, y) ((h:t) : r) == Vazio) || (locatePiece (x-1, y) ((h:t) : r) == Porta)
                 then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                 else if (locatePiece (x-1, y-1) ((h:t) : r) == Vazio) || (locatePiece (x-1, y-1) ((h:t) : r) == Porta)
                      then if b == True
                           then if (locatePiece (x-1, y-2) ((h:t) : r) == Vazio)
                                then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x-1,y-2) Caixa) (x,y-1) Vazio) (Jogador (x-1,y-1) d b))
                                else (Jogo ((h:t) : r) (Jogador (x,y) d b))
                           else (Jogo ((h:t) : r) (Jogador (x-1,y-1) d b))
                      else (Jogo ((h:t) : r) (Jogador (x,y) d b))
        Este -> if (locatePiece (x+1, y) ((h:t) : r) == Vazio) || (locatePiece (x+1, y) ((h:t) : r) == Porta)
                then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                else if (locatePiece (x+1, y-1) ((h:t) : r) == Vazio) || (locatePiece (x+1, y-1) ((h:t) : r) == Porta)
                     then if b == True
                           then if (locatePiece (x+1, y-2) ((h:t) : r) == Vazio)
                                then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x+1,y-2) Caixa) (x,y-1) Vazio) (Jogador (x+1,y-1) d b))
                                else (Jogo ((h:t) : r) (Jogador (x,y) d b))
                           else (Jogo ((h:t) : r) (Jogador (x+1,y-1) d b))
                     else (Jogo ((h:t) : r) (Jogador (x,y) d b))
    InterageCaixa -> case d of
        Oeste -> case b of
            False -> if locatePiece(x-1,y) ((h:t) : r) /= Caixa
                     then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                     else if (locatePiece(x,y-1) ((h:t) : r) /= Vazio) || (locatePiece(x-1,y-1) ((h:t) : r) /= Vazio)
                          then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                          else (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x-1,y) Vazio) (x,y-1) Caixa) (Jogador (x,y) d True))
            True -> if (locatePiece(x-1,y-1) ((h:t) : r) /= Vazio) || (locatePiece(x-1,y) ((h:t) : r) == Porta)
                    then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                    else if locatePiece(x-1,y) ((h:t) : r) /= Vazio
                         then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x-1,y-1) Caixa) (x,y-1) Vazio) (Jogador (x,y) d False))
                         else (Jogo (changePiece1 (changePiece1 ((h:t) : r) (checkFallPlace (x-1,y) ((h:t) : r)) Caixa) (x,y-1) Vazio) (Jogador (x,y) d False))
        Este -> case b of
            False -> if locatePiece(x+1,y) ((h:t) : r) /= Caixa
                     then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                     else if (locatePiece(x,y-1) ((h:t) : r) /= Vazio) || (locatePiece(x+1,y-1) ((h:t) : r) /= Vazio)
                          then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                          else (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x+1,y) Vazio) (x,y-1) Caixa) (Jogador (x,y) d True))
            True -> if (locatePiece(x+1,y-1) ((h:t) : r) /= Vazio) || (locatePiece(x+1,y) ((h:t) : r) == Porta)
                    then (Jogo ((h:t) : r) (Jogador (x,y) d b))
                    else if locatePiece(x+1,y) ((h:t) : r) /= Vazio
                         then (Jogo (changePiece1 (changePiece1 ((h:t) : r) (x+1,y-1) Caixa) (x,y-1) Vazio) (Jogador (x,y) d False))
                         else (Jogo (changePiece1 (changePiece1 ((h:t) : r) (checkFallPlace (x+1,y) ((h:t) : r)) Caixa) (x,y-1) Vazio) (Jogador (x,y) d False))  

{- | A função 'correrMovimentos' em combinação com a 'moveJogador' vai correr uma lista de movimentos sobre um jogo

-} 
             
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos x [] = x
correrMovimentos jogo (h:r) = correrMovimentos (moveJogador jogo h) r

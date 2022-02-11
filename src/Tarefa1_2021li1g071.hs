{- |
Module      : Tarefa1_2021li1g071
Description : Validação de um potencial mapa
Copyright   : André Castro Alves <a95033@alunos.uminho.pt>;
            : Nuno Igreja <a97433@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g071 where

import Tarefa3_2021li1g071
import LI12122

{- | A função 'estaOrganizado' verifica se uma lista de [(Peca, Coordenadas)] se encontra organizada (onde caso seja falso retorna False)

== Exemplos de utilização:
>>> estaOrganizado [(Bloco, (1,0)), (Bloco, (0,0))]
False
>>> estaOrganizado [(Bloco, (0,0)), (Bloco, (1,0))]
True
-}

estaOrganizado :: [(Peca, Coordenadas)] -> Bool
estaOrganizado [] = True
estaOrganizado [x] = True
estaOrganizado [(_, (a,b)) , (_, (as, bs))] | b > bs = False
                                            | (b == bs) && (a > as) = False
                                            | otherwise = True
estaOrganizado ((_, (a,b)) : (p, (as,bs)) : r) | b > bs = False
                                               | (b == bs) && (a > as) = False
                                               | otherwise = estaOrganizado ((p, (as,bs)) : r)

{- | A função 'organizar' vai organizar uma lista [(Peca, Coordenadas)] primeiramente pela coordenada y dos elementos e depois pela sua coordenada x

== Exemplos de utilização:
>>>organizar [(Bloco, (0,0)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (1,2))]
[(X,(0,0)),(X,(1,0)),(X,(0,2)),(X,(1,2))]
>>>organizar [(Bloco, (3,0)), (Bloco, (0,1)), (Bloco, (0,0)), (Bloco, (1,2))]
[(X,(3,0)),(X,(0,0)),(X,(0,1)),(X,(1,2))]

== Nota:
Mesmo a invocação anterior parecer dar um resultado falso, na função principal 'validaPotencialMapa' é chamada sempre a função 'estaOrganizado' para verificar se já se encontra organizada a lista
-}

organizar :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
organizar [] = []
organizar [(p, (a,b)) , (ps, (as,bs))] | b > bs = [(ps, (as,bs)) , (p, (a,b))]
                                       | (b == bs) && (a > as) =  [(ps, (as,bs)) , (p, (a,b))]
                                       | otherwise = [(p, (a,b)) , (ps, (as,bs))]
organizar ((p, (a,b)) : (ps, (as,bs)) : r) | b > bs = organizar((ps, (as,bs)) : (p, (a,b)) : r)
                                           | (b == bs) && (a > as) = organizar((ps, (as,bs)) : (p, (a,b)) : r)
                                           | otherwise = (p, (a,b)) : organizar ((ps, (as,bs)) : r)   

{- | A função 'locatePiece' vai retornar True caso a lista tenha um par de coordenadas igual ao par de coordenadas recebido com a função

== Exemplos de utilização:
>>>locatePiece (0,2) [(Bloco, (0,0)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (1,2))]
True
>>>locatePiece (1,0) [(Bloco, (0,0)), (Bloco, (2,0)), (Bloco, (0,1)), (Bloco, (1,1))]
False

-}                                           

locatePiece :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
locatePiece _ [] = False
locatePiece (x,y) ((_,(xs,ys)):r) | (x == xs) && (y == ys) = True
                                  | otherwise = locatePiece (x,y) r 

{- | A função 'returnPiece' em junção com a 'locatePiece' vai retornar o elemento da lista que terá um par de coordenadas igual ao par de coordenadas nos dado com a função

== Exemplos de utilização:
>>>returnPiece (0,2) [(Bloco, (0,0)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (1,2))]
(Bloco, (0,2))
>>>returnPiece (1,0) [(Bloco, (0,0)), (Bloco, (2,0)), (Bloco, (0,1)), (Bloco, (1,1))]
(Vazio, (1,0))

-}

returnPiece :: Coordenadas -> [(Peca, Coordenadas)] -> (Peca,Coordenadas)
returnPiece c [] = (Vazio, c)
returnPiece c ((p,(x,y)):r) | locatePiece c ((p,(x,y)):r) == False = (Vazio, c)
                            | c == (x,y)= (p,c)
                            | otherwise = returnPiece c r

{- | A função 'biggestYAxis0' vai retornar o maior elemento da primeira coluna do mapa final, ou seja a maior coordenada y dos pares de coordenadas com coordenada x igual a 0

== Exemplos de utilização:
>>>biggestYAxis0 [(Bloco, (0,0)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (1,2))]
2
>>>biggestYAxis0 [(Bloco, (0,0)), (Bloco, (2,0)), (Bloco, (0,1)), (Bloco, (1,1))]
1

-}

biggestYAxis0 :: [(Peca, Coordenadas)] -> Int
biggestYAxis0 [] = -1
biggestYAxis0 [(_, (0,b))] = b
biggestYAxis0 ((_,(x,y)):r) = if x == 0
                              then if y > biggestYAxis0 r
                                   then y
                                   else biggestYAxis0 r
                              else biggestYAxis0 r

{- | A função 'biggestX' vai retornar o comprimento do mapa final, ou seja a maior coordenada x da lista 

== Exemplos de utilização:
>>>biggestX [(Bloco, (0,0)), (Bloco, (0,2)), (Bloco, (1,0)), (Bloco, (1,2))]
1
>>>biggestX [(Bloco, (0,0)), (Bloco, (2,0)), (Bloco, (0,1)), (Bloco, (1,1))]
2

-}                              

biggestX :: [(Peca, Coordenadas)] -> Int 
biggestX [] = -1
biggestX [(_, (a,b))] = a
biggestX ((_,(x,y)):r) = if x > biggestX r
                         then x
                         else biggestX r

{- | A função 'coordenadaJaExistente' vai ao receber um par de coordenadas e uma lista de [(Peca, Coordenadas)] verificar se existem nessa lista algum elemento com um par de coordenadas igual ao par de coordenadas dado à função

== Exemplos de utilização:
>>>coordenadaJaExistente (0,0) [(Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
False
>>>coordenadaJaExistente (0,0) [(Bloco, (1,0)), (Bloco, (0,0)), (Bloco, (1,2))]
True

-}

coordenadaJaExistente :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
coordenadaJaExistente _ [] = False
coordenadaJaExistente (x,y) ((_,(xs,ys)):r) | x == xs && y == ys = True
                                            | otherwise = coordenadaJaExistente (x,y) r

{- | Esta função 'testePecasRepetidas' irá utilizar a função 'coordenadaJaExistente' para verificar se existem multiplas declarações da mesma coordenada na lista de [(Peca, Coordenadas)] nos atribuída, retornando True caso existam multiplas declarações da(s) mesma(s) coordenada(s)

== Exemplos de utilização:
>>>testePecasRepetidas [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
False
>>>testePecasRepetidas [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (1,2)), (Bloco, (1,2))]
True

-}

testePecasRepetidas :: [(Peca,Coordenadas)] -> Bool
testePecasRepetidas [] = False
testePecasRepetidas [x] = False
testePecasRepetidas ((_,l) : r) | coordenadaJaExistente l r == True = True
                                | otherwise = testePecasRepetidas r

{- | A função 'existePorta2' vai ao receber uma lista de [(Peca, Coordenadas)] verificar se existe uma porta nessa lista

== Exemplos de utilização:
>>>existePorta2 [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (1,2)), (Bloco, (1,2))]
False
>>>existePorta2 [(Porta, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
True

-}

existePorta2 :: [(Peca, Coordenadas)] -> Bool
existePorta2 [] = False
existePorta2 ((x, _) : r) | x == Porta = True
                          | otherwise = existePorta2 r

{- | A função 'existePorta1' vai ao receber uma lista de [(Peca, Coordenadas)] em combinação com a função 'existePorta2' verificar se existe uma e só uma porta nessa lista

== Exemplos de utilização:
>>>existePorta1 [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (1,2)), (Bloco, (1,2))]
False
>>>existePorta1 [(Porta, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
True
>>>existePorta1 [(Porta, (0,0)), (Porta, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
False

-}

existePorta1 :: [(Peca, Coordenadas)] -> Bool
existePorta1 [] = False
existePorta1 ((x, _) : r) | x == Porta = if existePorta2 r == True then False else True
                          | otherwise = existePorta1 r

{- | A função 'checkUnder' ao receber um par de coordenadas e uma lista de [(Peca, Coordenadas)] vai verificar dois pontos:

* Se existe nessa lista o par de coordenadas logo em baixo do par recebido pela função

* Caso o ponto acima seja verdade, se a Peca que se encontra nesse lugar não seja ou Vazio ou Porta

Caso passe em ambos os pontos irá retornar True, senão False

== Exemplos de utilização:
>>>checkUnder (0,0) [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
False
>>>checkUnder (0,0) [(Caixa, (0,0)), (Bloco, (1,0)), (Porta, (0,1)), (Bloco, (1,2))]
False
>>>checkUnder (0,0) [(Caixa, (0,0)), (Porta, (1,0)), (Bloco, (0,1)), (Bloco, (1,2))]
True

-}

checkUnder :: Coordenadas -> [(Peca, Coordenadas)] -> Bool
checkUnder _ [] = False
checkUnder (a,b) ((p,(x,y)) : r) 
                                 | (y == (b + 1)) && (a == x)= if (p == Porta) || (p == Vazio) then False else True
--                                 | a == (x-1) = not (p == (Porta))
                                 | otherwise = checkUnder (a,b) r

{- | A função 'caixaValida' irá em combinação com a função 'checkUnder' ver se, caso a lista recebida contenha uma caixa, essa caixa é válida. Uma caixa é válida se:

* A caixa se encontra em cima de uma outra caixa

* A caixa se encontra em cima de um bloco

== Exemplos de utilização:
>>>caixaValida [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
True
>>>caixaValida [(Caixa, (0,0)), (Bloco, (1,0)), (Bloco, (0,2)), (Bloco, (1,2))]
False
>>>caixaValida [(Caixa, (0,0)), (Porta, (1,0)), (Bloco, (0,1)), (Bloco, (1,2))]
True

-}

caixaValida :: [(Peca, Coordenadas)] -> Bool
caixaValida [] = True
caixaValida ((x,(a,b)) : r) | x == Caixa = if checkUnder (a,b) ((x,(a,b)) : r) == False then False else caixaValida r
                            | otherwise = caixaValida r

{- | A função 'mapaTemVaziosImp' vai ao receber uma lista de [(Peca, Coordenadas)] verificar se esta tem no mínimo uma peça vazio nele onde para isso vai testar por duas vertentes:

* Caso a peça seja Vazio

* Caso duas peças consecutivas na lista não sejam consecutivas no mapa final (por exemplo: [(Caixa, (0,0)), (Bloco, (2,0))] têm entre eles implicitamente uma peça Vazio) 

== Exemplos de utilização:
>>>mapaTemVaziosImp [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (0,1)), (Bloco, (1,1))]
False
>>>mapaTemVaziosImp [(Bloco, (0,0)), (Vazio, (1,0)), (Bloco, (0,1)), (Bloco, (1,1))]
True
>>>mapaTemVaziosImp [(Caixa, (0,0)), (Bloco, (2,0)), (Bloco, (0,1)), (Bloco, (1,1))]
True

-}

mapaTemVaziosImp :: [(Peca, Coordenadas)] -> Bool
mapaTemVaziosImp [] = False
mapaTemVaziosImp [(x,(a,b))] = x == Vazio
mapaTemVaziosImp [(x,(a,b)), (xs,(as,bs))] | x == Vazio = True
                                           | a /= as - 1 && b == bs = True
                                           | b == bs - 1 && as /= 0 = True
                                           | otherwise = xs == Vazio
mapaTemVaziosImp ((x,(a,b)): (xs,(as,bs)) : r) | x == Vazio = True
                                               | a /= as - 1 && b == bs = True
                                               | b == bs - 1 && as /= 0 = True
                                               | otherwise = mapaTemVaziosImp ((xs,(as,bs)) : r)

{- | A função 'checkUp' vai verificar de forma similar à 'checkRight' se existe uma base de blocos do mapa, porém não vai testar a peça de cima para não criar ciclos infinitos em algumas ocasiões

== Funções usadas:
* 'returnPiece'
* 'checkRight'
* 'checkUp'

-}  

checkUp :: Coordenadas -> [(Peca, Coordenadas)] -> Int -> Bool
checkUp _ [] _ = False
--checkUp l [(p,(a,b))] x 
checkUp (x,y) ((p,(a,b)) : r) l = if returnPiece (x+1, y) list == (Bloco, (x+1, y))
                                  then checkRight (x+1,y) list l
                                  else if returnPiece (x, y-1) list == (Bloco, (x, y-1))
                                       then checkUp (x, y-1) list l
                                       else if (returnPiece (x,y) list == (Bloco, (x,y))) && (x == l)
                                            then True
                                            else False
                               
          where list = ((p,(a,b)) : r)

{- | A função 'checkDown' vai verificar de forma similar à 'checkRight' se existe uma base de blocos do mapa, porém não vai testar a peça de cima para não criar ciclos infinitos em algumas ocasiões

== Funções usadas:
* 'returnPiece'
* 'checkRight'
* 'checkDown'

-}          

checkDown :: Coordenadas -> [(Peca, Coordenadas)] -> Int -> Bool
checkDown _ [] _ = False
checkDown (x,y) ((p,(a,b)) : r) l = if returnPiece (x+1, y) list == (Bloco, (x+1, y))
                                    then checkRight (x+1,y) list l
                                    else if returnPiece (x, y+1) list == (Bloco, (x, y+1))
                                         then checkDown (x, y+1) list l
                                         else if (returnPiece (x,y) list == (Bloco, (x,y))) && (x == l)
                                                   then True
                                                   else False
                               
          where list = ((p,(a,b)) : r)

{- | A função 'checkRight' vai verificar em três fase distintas se existe uma base de blocos do mapa:

1. Caso a peça à direita da que estamos a verificar for bloco, iremos chamar recursivamente a 'checkRight' a esse bloco

2. Se a peça não for um bloco, vamos ver se a peça a baixo da que estamos é bloco, onde se ela o for utilizamos a função 'checkDown' nessa peça

3. Se ambas as peças à direita e em baixo não for bloco, vamos verificar a que está em cima, onde caso ela seja bloco chamamos a função 'checkUp' a essa peça

Para realizar esta verificação de peças nós utilizamos a função 'returnPiece' dando-lhe o par de coordenadas e a lista que queremos verificar 

== Notas:
Com este método de verificação caso o mapa seja de tal forma construido que se cria um ciclo infinito nesta função, nas três funções de check ('checkRight', 'checkDown' e 'checkUp') se a peça que estamos a verificar se encontra na borda direita do mapa, retornamos True se a peça for um bloco e False se o contrário 

-}          

checkRight :: Coordenadas -> [(Peca, Coordenadas)] -> Int -> Bool
checkRight _ [] _ = False
checkRight (x,y) ((p,(a,b)) : r) l = if returnPiece (x+1, y) list == (Bloco, (x+1, y))
                                     then checkRight (x+1,y) list l
                                     else if returnPiece (x, y+1) list == (Bloco, (x, y+1))
                                          then checkDown (x, y+1) list l
                                          else if returnPiece (x, y-1) list == (Bloco, (x, y-1))
                                               then checkUp (x, y-1) list l
                                               else if (returnPiece (x,y) list == (Bloco, (x,y))) && (x == l)
                                                    then True
                                                    else False
                               
          where list = ((p,(a,b)) : r)

{- | A função 'baseBlocos' vai verificar se existe uma base constituída apenas por blocos na lista recebida, onde para isso vai verificar primeiramente se a peça que se encontra na coordenada y maior da primeira coluna é um bloco, onde caso o seja irá posteriormente ver a peça à sua direita

== Funções auxiliares utilizadas:
* 'returnPiece' para ver a peça que se encontra na coordenada pretendida
* 'biggestYAxis0' que irá devolver a coordenada y do elemento mais abaixo da primeira coluna
* 'biggestX' que irá devolver o comprimento do mapa resultante da lista que nos é dada
* 'checkRight' para verificar a peça à direita da primeira peça da base de blocos

== Exemplos de utilização:
>>>baseBlocos [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (0,1)), (Bloco, (1,1))]
True
>>>baseBlocos [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (1,1)), (Bloco, (2,1)), (Bloco, (3,1)), (Bloco, (3,0)), (Bloco, (4,0))]
True
>>>baseBlocos [(Bloco, (0,0)), (Bloco, (1,0)), (Bloco, (1,1)), (Bloco, (3,1)), (Bloco, (3,0)), (Bloco, (4,0))]
False

-}          

baseBlocos :: [(Peca, Coordenadas)] -> Bool
baseBlocos [] = False
baseBlocos ((p,(a,b)) : r) = if returnPiece (0, big) list /= (Bloco, (0, big))
                             then False
                             else if returnPiece (1, big) list == (Bloco, (1, big))
                                  then checkRight (1, big) list (biggestX list)
                                  else False
                               
          where big = biggestYAxis0 ((p,(a,b)) : r)
                list = ((p,(a,b)) : r) 

{- | Função que a Tarefa1 se encontra a implementar. A 'validaPotencialMapa' vai verificar se uma lista de [(Peca, Coordenadas)] consegue passar por 5 pontos distintos:

1. Testar se o mapa dispõe de peças repetidas, tal que irá utilizar a função 'testePecasRepetidas'

2. Verificar se existe uma, e só uma porta na lista de [(Peca, Coordenadas)], onde será utilizada a função 'existePorta1' para o suposto

3. Ver se caso existirem caixas na lista de [(Peca, Coordenadas)], essas mesmas são válidas, utilizando a função 'caixaValida' para o efeito

4. Verificar se existem (no mínimo um) espaços vazios na lista de [(Peca, Coordenadas)], sendo utilizada a função 'mapaTemVaziosImp' para o teste

5. Testar se a base do mapa é constituído apenas por blocos e se é uma linha conínua sem falácias, onde será utilizada a função 'baseBlocos' neste teste

== Notas:
* Para uma lista de [(Peca, Coordenadas)] ser válida terá que passar por todos os testes, onde caso falhe um deles a função retornará False e não testará os restantes casos
* A função 'testePecasRepetidas' é a única das 5 que se o caso de teste der falso irá retornar True
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa pecas 
                        | estaOrganizado pecas == False = validaPotencialMapa (organizar pecas)
                        | testePecasRepetidas pecas == True = False
                        | existePorta1 pecas == False = False
                        | caixaValida pecas == False = False
                        | mapaTemVaziosImp pecas == False = False
                        | baseBlocos pecas == False = False
                        | otherwise = True

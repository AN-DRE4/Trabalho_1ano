{- |
Module      : Tarefa5_2021li1g071
Description : Aplicação Gráfica

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Main where

import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import Tarefa2_2021li1g071
import Tarefa3_2021li1g071
import Tarefa4_2021li1g071
import LI12122
import Fixtures

-- | Estado de um 'ResultadoJogo'.
data ResultadoJogo
  = EmProgresso
  | MainMenu
  | JogoGanho
  | LoadJogo
  | FinalGame
  | PauseGame
  deriving (Read, Eq, Ord)

-- | Estado de um 'Jogo'.
data GameState =
  GameState
    Jogo 
    ResultadoJogo 
  deriving (Read, Eq)

background :: Color
background = makeColor 0.2 0.1 0.1 1

fps :: Int
fps = 60

game1 :: GameState
game1 = GameState m1e2 MainMenu

game2 :: GameState
game2 = GameState m2e2 MainMenu

game3 :: GameState
game3 = GameState m3e2 MainMenu

game4 :: GameState
game4 = GameState m4e2 MainMenu

game5 :: GameState
game5 = GameState m5e2 MainMenu

game6 :: GameState
game6 = GameState m6e2 MainMenu

game7 :: GameState
game7 = GameState m7e2 MainMenu

game8 :: GameState
game8 = GameState m8e2 MainMenu

game9 :: GameState
game9 = GameState m9e2 MainMenu

allGames :: IO[GameState]
allGames = return [game1, game2, game3, game4, game5, game6, game7, game8, game9]

displayMode :: Display
displayMode = FullScreen

loadIMG :: IO[Picture]
loadIMG = do parede <- loadJuicyPNG "sprites/Wall2.png"
             caixa <- loadJuicyPNG "sprites/crate2.png"
             porta <- loadJuicyPNG "sprites/door2.png"
             player_left <- loadJuicyPNG "sprites/player_left.png"
             player_right <- loadJuicyPNG "sprites/player_right.png"
             return (map fromJust[parede, caixa, porta, player_left, player_right])

pieceToPic :: (Peca,[Picture]) -> Picture
pieceToPic (Bloco,pics) = pics !! 0
pieceToPic (Caixa,pics) = pics !! 1
pieceToPic (Porta,pics) = pics !! 2
pieceToPic (Vazio,pics) = Blank

playerSide :: Direcao -> [Picture] -> Picture
playerSide x pics | x == Oeste = pics !! 3
                  | otherwise = pics !! 4

drawMap :: (Mapa,[Picture]) -> (Int,Int) -> (Int,Int) -> Direcao -> [Picture]
drawMap ([],_) _ _ _ = []
drawMap ((h:t),pics) (x,y) (xs,ys) d = (Translate 0 (fromIntegral(y*(-32))) (Pictures (drawLine2 (h,pics) x xs (if y == ys then 1 else 0) d))):(drawMap (t,pics) (x,y+1) (xs,ys) d)

drawLine2 :: ([Peca],[Picture]) -> Int -> Int -> Int -> Direcao -> [Picture]
drawLine2 ([],_) _ _ _ _ = []
drawLine2 ((h:t),pics) x xs a direcao = (Translate (fromIntegral(x*32)) 0 (if a == 1 && x == xs then playerSide direcao pics else pieceToPic (h,pics))):(drawLine2 (t,pics) (x+1) xs a direcao)

bigMapPic3 :: GameState -> [Picture] -> Picture
bigMapPic3 (GameState (Jogo mapa (Jogador coords direcao _)) state) pics | state == JogoGanho = pictures $ [color white $ Translate (-500) 0 $ Scale 0.6 0.6
                                                                                        (Text "Congratulations! You've won!"),
                                                                                        color white $ Translate (-500) (-120) $ Scale 0.4 0.4
                                                                                        (Text "Press enter to restart with a new map!"),
                                                                                        color white $ Translate (-500) (-240) $ Scale 0.4 0.4
                                                                                        (Text "Press tab to go back to the main menu!"),
                                                                                        color white $ Translate (-500) (-360) $ Scale 0.4 0.4
                                                                                        (Text "Press 'L' to choose a different level!")       
                                                                                        ]
                                                                         | state == FinalGame = pictures $ [color white $ Translate (-770) 0 $ Scale 0.5 0.5
                                                                                        (Text "Congratulations! You've completed the final map!"),
                                                                                        color white $ Translate (-300) (-120) $ Scale 0.4 0.4
                                                                                        (Text "Sit tight for new maps :))!"),
                                                                                        color white $ Translate (-500) (-240) $ Scale 0.4 0.4
                                                                                        (Text "Press enter to go back to the Main Menu!")      
                                                                                        ]
                                                                         | state == MainMenu = pictures $ [color white $ Translate (-250) 0 $ Scale 0.8 0.8
                                                                                        (Text "BlockDude"),
                                                                                        color white $ Translate (-350) (-120) $ Scale 0.4 0.4
                                                                                        (Text "Press enter to start playing!"),
                                                                                        color white $ Translate (-350) (-240) $ Scale 0.4 0.4
                                                                                        (Text "Press tab to choose a level!")
                                                                                        ]
                                                                         | state == LoadJogo = pictures $ [color white $ Translate (-120) 0 $ Scale 0.8 0.8
                                                                                        (Text "Levels"),
                                                                                        color white $ Translate (-350) (-120) $ Scale 0.4 0.4
                                                                                        (Text "Pick a level from 1 to 9!")
                                                                                        ]
                                                                         | state == PauseGame = pictures $ [color white $ Translate (-250) 0 $ Scale 0.8 0.8
                                                                                        (Text "Pause"),
                                                                                        color white $ Translate (-350) (-120) $ Scale 0.4 0.4
                                                                                        (Text "Enter to continue"),
                                                                                        color white $ Translate (-350) (-240) $ Scale 0.4 0.4
                                                                                        (Text "R to restart"),
                                                                                        color white $ Translate (-350) (-360) $ Scale 0.4 0.4
                                                                                        (Text "Tab to go back to menu")
                                                                                        ]
                                                                         | otherwise = pictures $ [translate (-315) 315 (scale 1.5 1.5 (pictures (drawMap (mapa, pics) (0,0) coords direcao))),
                                                                                      color white $ Translate (350) (420) $ Scale 0.4 0.4
                                                                                        (Text "Press P to pause")]

nextGame :: Jogo -> [GameState] -> Maybe GameState
nextGame game [] = Nothing
nextGame (Jogo mapa player) ((GameState(Jogo mapa1 player1)_):r) | head mapa == head mapa1 = Just (head r)
                                                                 | otherwise = nextGame (Jogo mapa player) r

restartGame :: GameState -> [GameState] -> GameState
restartGame (GameState(Jogo mapa player)a) ((GameState(Jogo mapa1 player1)_):r) | head mapa == head mapa1 = GameState(Jogo mapa1 player1) EmProgresso
                                                                                | otherwise = restartGame (GameState(Jogo mapa player)a) r

alterMode :: GameState -> ResultadoJogo -> GameState
alterMode (GameState jogo modo) newmodo = GameState jogo newmodo 

gameMap :: GameState -> Mapa
gameMap (GameState(Jogo mapa1 player1)_) = mapa1

handleKeys :: Event -> [GameState] -> GameState -> GameState
handleKeys event games (GameState jogo estado)  
  | estado == JogoGanho = case event of (EventKey (SpecialKey KeyEnter) Down _ _) -> if head(gameMap(GameState jogo estado)) /= head(gameMap game9)
                                                                                     then alterMode (fromJust(nextGame jogo games)) EmProgresso
                                                                                     else GameState m1e2 FinalGame
                                        (EventKey (SpecialKey KeyTab) Down _ _) -> GameState m1e2 MainMenu
                                        (EventKey (Char 'l') Down _ _) -> GameState m1e2 LoadJogo
                                        _ -> GameState jogo estado
  | estado == MainMenu = case event of (EventKey (SpecialKey KeyEnter) Down _ _) -> GameState jogo EmProgresso
                                       (EventKey (SpecialKey KeyTab) Down _ _) -> GameState jogo LoadJogo                                       
                                       _ -> GameState jogo estado
  | estado == LoadJogo = case event of (EventKey (Char '1') Down _ _) -> alterMode (head games) EmProgresso
                                       (EventKey (Char '2') Down _ _) -> alterMode (games !! 1) EmProgresso
                                       (EventKey (Char '3') Down _ _) -> alterMode (games !! 2) EmProgresso
                                       (EventKey (Char '4') Down _ _) -> alterMode (games !! 3) EmProgresso
                                       (EventKey (Char '5') Down _ _) -> alterMode (games !! 4) EmProgresso 
                                       (EventKey (Char '6') Down _ _) -> alterMode (games !! 5) EmProgresso
                                       (EventKey (Char '7') Down _ _) -> alterMode (games !! 6) EmProgresso 
                                       (EventKey (Char '8') Down _ _) -> alterMode (games !! 7) EmProgresso
                                       (EventKey (Char '9') Down _ _) -> alterMode (games !! 8) EmProgresso                                         
                                       _ -> GameState jogo estado
  | estado == FinalGame = case event of (EventKey (SpecialKey KeyEnter) Down _ _) -> GameState jogo MainMenu                                
                                        _ -> GameState jogo estado
  | estado == PauseGame = case event of (EventKey (SpecialKey KeyEnter) Down _ _) -> alterMode (GameState jogo estado) EmProgresso                                
                                        (EventKey (SpecialKey KeyTab) Down _ _) -> GameState jogo MainMenu 
                                        (EventKey (Char 'r') Down _ _) -> restartGame (GameState jogo estado) games
                                        _ -> GameState jogo estado
  | otherwise = case event of (EventKey (SpecialKey KeyLeft) Down _ _) -> GameState (moveJogador jogo AndarEsquerda) estado 
                              (EventKey (SpecialKey KeyRight) Down _ _) -> GameState (moveJogador jogo AndarDireita) estado
                              (EventKey (SpecialKey KeyUp) Down _ _) -> GameState (moveJogador jogo Trepar) estado
                              (EventKey (SpecialKey KeySpace) Down _ _) -> GameState (moveJogador jogo InterageCaixa) estado
                              (EventKey (Char 'p') Down _ _) -> alterMode (GameState jogo estado) PauseGame
                              _ -> GameState jogo estado

playerLocat :: Jogo -> (Int, Int)
playerLocat (Jogo mapa (Jogador coords direcao _)) = coords

whatLocation :: [(Peca, Coordenadas)] -> Peca -> (Int, Int)
whatLocation ((h, t) : r) piece | h == piece = t
                                | otherwise = whatLocation r piece

doorLocat :: Jogo -> (Int, Int)
doorLocat (Jogo mapa (Jogador coords direcao _)) = whatLocation (desconstroiMapa mapa) Porta

update :: Float -> GameState -> GameState
update f (GameState (Jogo mapa player) estado) | playerLocat (Jogo mapa player) == doorLocat (Jogo mapa player) = GameState (Jogo mapa player) JogoGanho
                                               | otherwise = GameState (Jogo mapa player) estado

main :: IO()
main = do loadedIMG <- loadIMG
          games <- allGames
          play displayMode background fps (head games) (`bigMapPic3` loadedIMG) (`handleKeys` games) update

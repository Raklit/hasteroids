{-# language DataKinds #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.Printf
import GameState


width, height, offset, fps :: Int
width = 640
height = 480
offset = 100
fps = 60

window :: Display
window = InWindow "Hasteroids" (width, height) (offset, offset)

background :: Color
background = black

-- draw game objects

ship :: GameObject -> Picture
ship gameObject =
  pictures [
    translate x y $ color white $ circleSolid size,
    translate (x + dx) (y + dy)  $ color black $ circleSolid (size / 3)
  ]
  where
    (PhysObject (Point2D x y) (Point2D vx vy) size angle) = physObject gameObject
    Point2D dx dy = rotatePoint (Point2D 0.0 (size / 2)) angle

bullet :: GameObject -> Picture
bullet gameObject =
  pictures [
    translate x y $ color white $ circleSolid size
  ]
  where
    (PhysObject (Point2D x y) (Point2D vx vy) size angle) = physObject gameObject

asteroid :: GameObject -> Picture
asteroid gameObject =
  pictures [
    translate x y $ color white $ circle size
  ]
  where
    (PhysObject (Point2D x y) (Point2D vx vy) size angle) = physObject gameObject

drawObject :: GameObject -> Picture
drawObject gameObject
  | type' == "ship" = ship gameObject
  | type' == "asteroid" = asteroid gameObject
  | otherwise = bullet gameObject
  where
      type' = t gameObject

drawObjects :: GameState -> Picture
drawObjects gameState = pictures (map drawObject (objects gameState))

-- draw HUD

drawBorder :: Picture
drawBorder = pictures [
    color white $ line [
      (GameState.leftBound, GameState.upBound), (GameState.rightBound, GameState.upBound),
      (GameState.rightBound, GameState.downBound), (GameState.leftBound, GameState.downBound),
      (GameState.leftBound, GameState.upBound)
    ]
  ]

drawGameInfo :: GameState -> Picture
drawGameInfo gameState =
  pictures [
    translate 180 200 $ color white $ scale 0.1 0.1 $ text labelScore,
    translate 180 180 $ color white $ scale 0.1 0.1 $ text labelHealth
  ]
  where
    player = head (objects gameState)
    score' = score gameState
    health' = health player
    labelScore =  printf "SCORE:  %05d" score'
    labelHealth = printf "HEALTH: %05d" health'

drawGameControls :: Picture
drawGameControls =
  pictures [
    translate 180 (-140) $ color white $ scale 0.1 0.1 $ text labelControls,
    translate 180 (-160) $ color white $ scale 0.1 0.1 $ text labelFowardBack,
    translate 180 (-180) $ color white $ scale 0.1 0.1 $ text labelRotate,
    translate 180 (-200) $ color white $ scale 0.1 0.1 $ text labelFire
  ]
  where
    labelFowardBack = "W/S - THRUST"
    labelRotate = "A/D - ROTATE"
    labelFire = "L - FIRE"
    labelControls = "CONTROLS:"

drawSceneInfo :: GameState -> Picture
drawSceneInfo gameState =
  pictures [
    translate 180 20 $ color white $ scale 0.1 0.1 $ text labelAsteroids,
    translate 180 0 $ color white $ scale 0.1 0.1 $ text labelWaveTime,
    translate 180 (-20) $ color white $ scale 0.1 0.1 $ text labelReload
  ]
  where
    labelAsteroids = printf "ASTEROIDS: %05d" asteroidsCount
    labelWaveTime =  printf "NEXT WAVE: %05.1f" waveTime
    labelReload   =  printf "RELOAD TIME: %05.1f" reloadTime
    asteroidsCount = length (filter (\object -> t object == "asteroid") (objects gameState) )
    waveTime = timeBeforeNextWave gameState
    reloadTime = timeBeforeNextFire gameState


-- draw game over screen
drawGameOver :: GameState -> Picture
drawGameOver gameState = 
  pictures [
    translate (-100) 20 $ color white $ scale 0.1 0.1 $ text labelGameOver,
    translate (-100) 0 $ color white $ scale 0.1 0.1 $ text labelScore,
    translate (-100) (-20) $ color white $ scale 0.1 0.1 $ text labelRestart
  ]
  where
    score' = score gameState
    labelScore =  printf "SCORE:  %05d" score'
    labelGameOver = "GAME OVER!"
    labelRestart = "PRESS R FOR RESTART"

-- draw scene

draw :: GameState -> Picture
draw gameState = 
  if gameOver' then
    drawGameOver gameState
  else
    pictures [
      drawObjects gameState,
      drawBorder,
      drawGameInfo gameState,
      drawGameControls,
      drawSceneInfo gameState
    ]
  where
    gameOver' = gameOver gameState



-- handlers and init

initialState :: GameState
initialState = GameState.initGameState

handleAction :: Event -> GameState -> GameState
handleAction (EventKey (Char 'w') Down _ _) gameState = GameState.setFowardPressed True gameState
handleAction (EventKey (Char 's') Down _ _) gameState = GameState.setBackPressed True gameState

handleAction (EventKey (Char 'a') Down _ _) gameState = GameState.setLeftPressed True gameState
handleAction (EventKey (Char 'd') Down _ _) gameState = GameState.setRightPressed True gameState

handleAction (EventKey (Char 'l') Down _ _) gameState = GameState.setFirePressed True gameState


handleAction (EventKey (Char 'w') Up _ _) gameState = GameState.setFowardPressed False gameState
handleAction (EventKey (Char 's') Up _ _) gameState = GameState.setBackPressed False gameState

handleAction (EventKey (Char 'a') Up _ _) gameState = GameState.setLeftPressed False gameState
handleAction (EventKey (Char 'd') Up _ _) gameState = GameState.setRightPressed False gameState

handleAction (EventKey (Char 'l') Up _ _) gameState = GameState.setFirePressed False gameState

handleAction (EventKey (Char 'r') Up _ _) gameState = 
  if gameOver' then
    GameState.initGameState
  else
    gameState
  where
    gameOver' = gameOver gameState

handleAction _ gameState = gameState

update :: Float -> GameState -> GameState
update = GameState.gameLoop


showWindow :: IO()
showWindow = play window background fps initialState draw handleAction update

main :: IO()
main = showWindow
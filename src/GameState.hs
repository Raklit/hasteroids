{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
module GameState
  where
import Graphics.Gloss.Geometry.Angle (degToRad)
import Control.Lens
import Data.List

data GameState = GameState {score :: Int, timeBeforeNextWave :: Float, timeBeforeNextFire :: Float, rndSeed :: Int, gameOver :: Bool, objects :: [GameObject], controlState :: ControlState}
  deriving (Show)
data Point2D = Point2D {x :: Float, y :: Float}
  deriving (Show)
data PhysObject = PhysObject {position :: Point2D, velocity :: Point2D, size :: Float, angle :: Float}
  deriving (Show)
data GameObject = GameObject {t :: String, health :: Int, lifeTime :: Float, physObject :: PhysObject}
  deriving (Show)
data ControlState = ControlState {forward :: Bool, back :: Bool, left :: Bool, right :: Bool, fire :: Bool}
  deriving (Show)

cheats :: Bool
cheats = False

forwardSpeed, rotationSpeed :: Float
forwardSpeed = 50.0
rotationSpeed = 5.0
maxPlayerHealth :: Int
maxPlayerHealth = 3

-- two main functions

initGameState :: GameState
initGameState = state
        where
            playerPhysObject = PhysObject {position = Point2D {x = startX, y = startY}, velocity = Point2D {x = 0, y = 0}, size = 30, angle = 0.0}
            player = GameObject {t = "ship", health = maxPlayerHealth, lifeTime = -1.0, physObject = playerPhysObject}
            controlState = ControlState {forward = False, back = False, left = False, right = False, fire = False}
            state = GameState {score = 0, timeBeforeNextWave = newWaveTime, rndSeed = 0, timeBeforeNextFire = 0, gameOver = False, objects = [player], controlState = controlState}
            startX = (rightBound + leftBound) / 2
            startY = (upBound + downBound) / 2

gameLoop :: Float -> GameState -> GameState
gameLoop seconds gameState = finalStage
  where
    inputStage = inputReact gameState
    corrosionStage = objectsCorrosion seconds inputStage
    calcTimerStage = calcTimer seconds corrosionStage
    waveStage = wavesHandler calcTimerStage
    collisionStage = collisionsHandler waveStage
    removeStage = removeDeadthObjects collisionStage
    moveStage = moveObjects seconds removeStage
    wrapStage = wrapObjects moveStage
    gameOverStage = gameOverHandler wrapStage
    finalStage = gameOverStage

-- manage game over

gameOverHandler :: GameState -> GameState
gameOverHandler gameState =
  if health' == 0 then gameState{gameOver = True, objects = [player]} else gameState
  where
    player = head (objects gameState)
    health' = health player

-- manage death of objects

decObjectLifeTime :: Float -> GameObject -> GameObject
decObjectLifeTime seconds gameObject = gameObject{lifeTime = newLifeTime}
  where
    lifeTime' = lifeTime gameObject
    decLifeTime = lifeTime' - seconds
    normDecLifeTime = if decLifeTime < 0.0 then 0.0 else decLifeTime
    newLifeTime = if lifeTime' > 0.0 then normDecLifeTime else lifeTime'

decObjectsLifeTime :: Float -> GameState -> GameState
decObjectsLifeTime seconds gameState = gameState { objects = map (decObjectLifeTime seconds) (objects gameState) }

killExpiredObject :: GameObject -> GameObject
killExpiredObject gameObject = newGameObject
  where
    lifeTime' = lifeTime gameObject
    health' = health gameObject
    newHealth = if lifeTime' == 0.0 then 0 else health'
    newGameObject = gameObject {health = newHealth}

killExpiredObjects :: GameState -> GameState
killExpiredObjects gameState = gameState { objects = map killExpiredObject (objects gameState) }


objectsCorrosion :: Float -> GameState -> GameState
objectsCorrosion seconds gameState = killExpiredObjects (decObjectsLifeTime seconds gameState)

removeDeadthObjects :: GameState -> GameState
removeDeadthObjects gameState = gameState {objects = newObjects}
  where
    player:others = objects gameState
    newObjects =  player : filter (\object -> health object > 0) others


-- manage wave

newWaveTime, asteroidLifeTime, asteroidsSize, asteroidsSpeed :: Float
newWaveTime = 5.0
asteroidLifeTime = 30.0
asteroidsSize = 15.0
asteroidsSpeed = 25.0
asteroidsPerWave, asteroidsHealth :: Int
asteroidsPerWave = 5
asteroidsHealth = 1

calcTimer :: Float -> GameState -> GameState
calcTimer seconds gameState = gameState{timeBeforeNextWave = newTimeBeforeNextWave, timeBeforeNextFire = newTimeBeforeNextFire}
  where
    timeBeforeNextWave' = timeBeforeNextWave gameState
    timeBeforeNextFire' = timeBeforeNextFire gameState
    decTimeBeforeNextWave = timeBeforeNextWave' - seconds
    decTimeBeforeNextFire = timeBeforeNextFire' - seconds
    newTimeBeforeNextWave = if decTimeBeforeNextWave < 0.0 then 0.0 else decTimeBeforeNextWave
    newTimeBeforeNextFire = if decTimeBeforeNextFire < 0.0 then 0.0 else decTimeBeforeNextFire

wavesHandler :: GameState -> GameState
wavesHandler gameState = finalStage
  where
    timeBeforeNextWave' = timeBeforeNextWave gameState
    gameOver' = gameOver gameState
    tempStage = (spawnAsteroids gameState){timeBeforeNextWave = newWaveTime}
    spawnStage = if timeBeforeNextWave' == 0 && not gameOver' then tempStage else gameState
    finalStage = spawnStage

-- ugly random generator

rndA, rndC, rndM :: Int
rndA = 4096
rndC = 150889
rndM = 714025

rndGenerator :: GameState -> (Int, GameState)
rndGenerator gameState = (newSeed, gameState {rndSeed = newSeed})
  where
    rndSeed' = rndSeed gameState
    newSeed = (rndA*rndSeed' + rndC) `mod` rndM

-- spawners

bulletsLifeTime, bulletsSpeed, bulletsSize, newBulletsTime :: Float
bulletsLifeTime = 5.0
bulletsSpeed = 100.0
bulletsSize = 10.0
newBulletsTime = 1

spawnBullet :: GameState -> GameState
spawnBullet gameState = newState
  where
    objects' = objects gameState
    player = head objects'
    playerPhysObject = physObject player
    playerSize = size playerPhysObject
    playerAngle = angle playerPhysObject
    newObjects = objects' ++ [bullet]
    Point2D dx dy = rotatePoint (Point2D 0.0 (playerSize + bulletsSize + 1.0)) playerAngle
    Point2D x y = position playerPhysObject
    nx = x + dx
    ny = y + dy
    Point2D vx vy = rotatePoint (Point2D 0.0 bulletsSpeed) playerAngle
    bulletPhysObject = playerPhysObject {position = Point2D nx ny, velocity = Point2D vx vy, size = bulletsSize}
    bullet = GameObject {t = "bullet", health = 1, lifeTime = bulletsLifeTime, physObject = bulletPhysObject}
    newState = gameState {objects = newObjects, timeBeforeNextFire = newBulletsTime}

spawnAsteroids :: GameState -> GameState
spawnAsteroids gameState = newState
  where
    objects' = objects gameState
    newObjects = objects' ++ [newAsteroid]
    newAsteroid = GameObject {t = "asteroid", health = asteroidsHealth, lifeTime = asteroidLifeTime, physObject = newAsteroidPhysObject}
    newAsteroidPhysObject = PhysObject {position = Point2D {x = rndX, y = rndY}, velocity = Point2D {x = vx, y = vy}, size = asteroidsSize, angle = rndAngle}

    (xCoin, xCoinState) = rndGenerator gameState
    (yCoin, yCoinState) = rndGenerator xCoinState
    tempM = fromIntegral (rndM - 1)
    normXCoin = fromIntegral xCoin / tempM
    normYCoin = fromIntegral yCoin / tempM

    rndX = if normXCoin > 0.5 then rightBound else leftBound
    rndY = if normYCoin > 0.5 then upBound else downBound
    rndAngle = (normXCoin + normYCoin) * 180
    Point2D vx vy = rotatePoint (Point2D 0.0 asteroidsSpeed) rndAngle

    newSeed = yCoin
    newState = gameState {objects = newObjects, rndSeed = newSeed}

-- manage collisions

collisionsHandler :: GameState -> GameState
collisionsHandler gameState = newState
  where
    objects' = objects gameState
    player = head objects'
    idxList = [0..length objects' - 1]
    bullets = filter (\id -> t (objects' !! id) == "bullet") idxList
    asteroids = filter (\id -> t (objects' !! id) == "asteroid") idxList
    collidedObjects = getIndexesOfCollidedBulletsAndAsteroids bullets asteroids objects'
    tempObjects = decHealthOfCollidedObjects collidedObjects objects'
    destroydedAsteroidsCount = length (filter (\object -> t object == "asteroid" && health object == 0) tempObjects)
    score' = score gameState
    newScore = score' + destroydedAsteroidsCount
    collidedWithPlayer = filter (\item -> checkCollide 0 item tempObjects) (tail idxList)
    playerDamage = takePlayerDamage collidedWithPlayer
    playerHealth = health player
    decPlayerHealth = playerHealth - playerDamage
    newHealth = if decPlayerHealth < 0 then 0 else decPlayerHealth
    newPlayer = player {health = newHealth}
    temp2Objects = destroyCollidedObjects collidedWithPlayer tempObjects
    newObjects = newPlayer:tail temp2Objects
    newState = gameState {score = newScore, objects = newObjects}

getIndexesOfCollidedBulletsAndAsteroids :: [Int] -> [Int] -> [GameObject] -> [Int]
getIndexesOfCollidedBulletsAndAsteroids bulletsIdx asteroidsIdx objects = collidedObjects
  where
    bullets_asteroids = [[x,y] | x <- bulletsIdx, y <- asteroidsIdx]
    collidedObjectsPairs = filter (\item -> checkCollide (head item) (item !! 1) objects) bullets_asteroids
    collidedObjects = nub (sort (concat collidedObjectsPairs) )

decHealthOfCollidedObjects :: [Int] -> [GameObject] -> [GameObject]
decHealthOfCollidedObjects collidedObjects objects = newObjects
  where
    newObjects = foldl decHealthOfObjectById objects collidedObjects

decHealthOfObjectById :: [GameObject] -> Int -> [GameObject]
decHealthOfObjectById objects id = objects & element id .~ object{health = newHealth}
  where
    object = objects !! id
    decHealth = health object - 1
    newHealth = if decHealth < 0 then 0 else decHealth

destroyCollidedObjects :: [Int] -> [GameObject] -> [GameObject]
destroyCollidedObjects collidedObjects objects = newObjects
  where
    newObjects = foldl destroyObjectById objects collidedObjects

takePlayerDamage :: [Int] -> Int
takePlayerDamage collidedObjects =
  if cheats then 0 else length collidedObjects


destroyObjectById :: [GameObject] -> Int -> [GameObject]
destroyObjectById objects id = objects & element id .~ object{health = 0}
  where
    object = objects !! id

checkCollide :: Int -> Int -> [GameObject] -> Bool
checkCollide a b x = distBtwnObjects < minDist
  where
    objectA = physObject (x !! a)
    objectB = physObject (x !! b)
    Point2D xa ya = position objectA
    Point2D xb yb = position objectB
    sizea = size objectA
    sizeb = size objectB
    minDist = sizea + sizeb
    distBtwnObjects = sqrt((xb - xa)^2 + (yb - ya)^2)

-- move objects

moveObject :: Float -> GameObject -> GameObject
moveObject seconds gameObject@GameObject {physObject} = newGameObject
    where
        newGameObject = gameObject {physObject = physObject {position = Point2D nx ny}}
        Point2D x y = position physObject
        Point2D vx vy = velocity physObject
        nx = x + vx * seconds
        ny = y + vy * seconds

moveObjects :: Float -> GameState -> GameState
moveObjects seconds gameState = gameState { objects = map (moveObject seconds) (objects gameState) }

-- input react

inputReact :: GameState -> GameState
inputReact gameState =
  if gameOver' then
    gameState
  else 
    inputFireReact (inputMovementReact gameState)
  where
    gameOver' = gameOver gameState

inputFireReact :: GameState -> GameState
inputFireReact gameState
  | startFire = spawnBullet gameState
  | otherwise = gameState
  where
    gameControlState = controlState gameState
    timeBeforeNextFire' = timeBeforeNextFire gameState
    fireState = fire gameControlState
    startFire = fireState && (timeBeforeNextFire' == 0.0 || cheats)

inputMovementReact :: GameState -> GameState
inputMovementReact gameState = inputXReact (inputYReact gameState)

inputXReact :: GameState -> GameState
inputXReact gameState
  | leftState = rotatePlayer False gameState
  | rightState = rotatePlayer True gameState
  | otherwise = gameState
  where
      gameControlState = controlState gameState
      leftState = left gameControlState
      rightState = right gameControlState

inputYReact :: GameState -> GameState
inputYReact gameState
  | forwardState = playerThurst True gameState
  | backState = playerThurst False gameState
  | otherwise = dropPlayerVelocity gameState
  where
      gameControlState = controlState gameState
      forwardState = forward gameControlState
      backState = back gameControlState

-- check bounds

upBound, downBound, leftBound, rightBound :: Float
downBound = -225.0
upBound = 225.0
leftBound = -300.0
rightBound = 150.0

teleportOffset :: Float
teleportOffset = 35.0

wrapObject :: GameObject -> GameObject
wrapObject gameObject@GameObject {physObject} =
    gameObject {physObject = physObject { position = Point2D correctX correctY } }
    where
        PhysObject {position = (Point2D x y), size} = physObject

        fieldWidth = 450.0
        fieldHeight = 450.0

        correctX
          | (x - size) < leftBound = rightBound - size - teleportOffset
          | (x + size) > rightBound = leftBound + size + teleportOffset
          | otherwise = x

        correctY
          | (y - size) < downBound = upBound - size - teleportOffset
          | (y + size) > upBound = downBound + size + teleportOffset
          | otherwise = y

wrapObjects :: GameState -> GameState
wrapObjects gameState = gameState {
    objects = map wrapObject (objects gameState)
}

-- input events

setLeftPressed :: Bool -> GameState -> GameState
setLeftPressed value gameState@GameState {controlState} =
  gameState { controlState = controlState { left = value } }

setRightPressed :: Bool -> GameState -> GameState
setRightPressed value gameState@GameState {controlState} =
  gameState { controlState = controlState { right = value } }

setFowardPressed :: Bool -> GameState -> GameState
setFowardPressed value gameState@GameState {controlState} =
  gameState { controlState = controlState { forward = value } }

setBackPressed :: Bool -> GameState -> GameState
setBackPressed value gameState@GameState {controlState} =
  gameState { controlState = controlState { back = value } }

setFirePressed :: Bool -> GameState -> GameState
setFirePressed value gameState@GameState {controlState} =
  gameState { controlState = controlState { fire = value } }

-- move character

playerThurst :: Bool -> GameState -> GameState
playerThurst isForward gameState = newState
    where
        player = head (objects gameState)
        playerPhysObject = physObject player
        (PhysObject (Point2D x y) (Point2D vx vy) _ angle) = playerPhysObject
        newState = gameState {objects = player' : tail (objects gameState)}
        player' = player {physObject = playerPhysObject {velocity = velocity' }}
        velocity' = rotatePoint (Point2D 0.0 speed) angle
        speed = if isForward then forwardSpeed else (-forwardSpeed)

wrapAngle :: Float -> Float
wrapAngle x
  | x < 0.0 = 360.0 + x
  | x > 360.0 = x - 360.0
  | otherwise = x

rotatePlayer :: Bool -> GameState -> GameState
rotatePlayer clockwise gameState = newState
    where
        player = head (objects gameState)
        playerPhysObject = physObject player
        (PhysObject (Point2D x y) (Point2D vx vy) size angle) = playerPhysObject
        rotationSpeed' = if clockwise then rotationSpeed else -rotationSpeed
        newAngle = wrapAngle $ angle + rotationSpeed'
        player' = player {physObject = playerPhysObject {angle = newAngle}}
        newState = gameState {objects = player' : tail (objects gameState)}

-- drop player velocity

dropPlayerVelocity :: GameState -> GameState
dropPlayerVelocity gameState = newState
    where
        player = head (objects gameState)
        playerPhysObject = physObject player
        Point2D x y = velocity playerPhysObject
        newState = gameState {objects =
            player {physObject = playerPhysObject {velocity = Point2D 0.0 0.0}} :
            tail (objects gameState)}

-- help func

rotatePoint :: Point2D -> Float -> Point2D
rotatePoint (Point2D x y) angle = Point2D (x * cos rad + y * sin rad) (- x * sin rad + y * cos rad)
    where
        rad = degToRad angle

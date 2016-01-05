module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Text exposing (..)
import Config exposing (..)
import Ship exposing (..)
import Bullet exposing (..)
import Rock exposing (..)
import Physics

-- MODEL

type alias Game =
  { ship : Ship
  , bullets: List Bullet
  , rocks : List Rock
  }


initGame : Game
initGame =
  let
    rockPositions =
      ( { x = 50, y = 45 }
      , { x = -134, y = 208 }
      )
    rockVelocities =
      ( { x = 4, y = -9 }
      , { x = -13, y = 8 }
      )
  in
  { ship = initShip
  , bullets = []
  , rocks =
    [ initRock 3 25 (fst rockVelocities) (fst rockPositions)
    , initRock 3 9 (snd rockVelocities) (snd rockPositions)
    ]
  }


-- UPDATE

update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    bullets = fireNewBullet game.ship game.bullets

    d1 = Debug.watch "rocks1" game.rocks

    collisionTests = List.map
      (detectCollisions game.rocks) bullets

    newBullets = bullets
      |> List.filterMap (updateBullet dt)
      |> removeDeadBullets (onTargetBullets collisionTests)

    newRocks = game.rocks
      |> List.map2 (updateRock dt) (damagedRocks collisionTests)
      |> List.concat

    d2 = Debug.watch "rocks2" newRocks
  in
  { game
  | ship = updateShip (dt, keyInput, fireInput) game.ship
  , bullets = newBullets
  , rocks = newRocks
  }


fireNewBullet : Ship -> List Bullet -> List Bullet
fireNewBullet ship bullets =
  if ship.firing then
    (initBullet ship) :: bullets
  else
    bullets

hitAnyTarget : List (Physics.CollisionResult a) -> Bool
hitAnyTarget =
  List.any (\n -> n.result == True)


onTargetBullets : List (List (Physics.CollisionResult a)) -> List Bool
onTargetBullets collisionTests =
  List.map hitAnyTarget collisionTests


damagedRocks : List (List (Physics.CollisionResult a)) -> List Bool
damagedRocks collisionTests =
  let
    ct = List.map hitAnyTarget (transpose collisionTests)
  in
  if List.length ct < 1 then [False, False] else ct


transpose : List (List a) -> List (List a)
transpose ll =
  case ll of
    [] -> []
    ([]::xss) -> transpose xss
    ((x::xs)::xss) ->
      let
        heads = List.filterMap List.head xss
        tails = List.filterMap List.tail xss
      in
        (x::heads)::transpose (xs::tails)


removeDeadBullets : List Bool -> List Bullet -> List Bullet
removeDeadBullets hits bullets =
  let
    zipped = List.map2 (,) hits bullets
    rmv a = if (fst a) then Nothing else Just (snd a)
  in
  List.filterMap rmv zipped


detectCollisions : List (Physics.Collidable a) -> Bullet
  -> List (Physics.CollisionResult a)
detectCollisions targets bullet =
  List.map (Physics.collides bullet) targets


-- VIEW

view : Game -> Element
view game =
  let
    background = rect gameWidth gameHeight
      |> filled lightBlue
    theShip = drawShip game.ship
    allForms = List.concat
      [ [ background, theShip ]
      , List.map drawBullet game.bullets
      , List.map drawRock game.rocks
      --, [ viewGameState game ]
      ]
  in
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight allForms


viewGameState : Game -> Form
viewGameState game =
  show game
    |> toForm
    |> move (-halfWidth,0)


-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update initGame inputSignal


inputSignal : Signal (Float, KeyInput, Bool)
inputSignal =
  let
    delta = fps 30
    tuples = Signal.map3 (,,) delta Keyboard.arrows Keyboard.space
  in
  Signal.sampleOn delta tuples


main : Signal Element
main =
  Signal.map view gameState
  --Signal.map show gameState

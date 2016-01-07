module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)

import UI exposing (gameWidth, gameHeight, KeyInput, ui)
import Ship exposing (Ship)
import Bullet exposing (Bullet)
import Rock exposing (Rock)
import Scoreboard exposing (Scoreboard)
import Physics


-- MODEL

type alias Game =
  { ship : Ship
  , bullets: List Bullet
  , rocks : List Rock
  , scoreboard: Scoreboard
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
  { ship = Ship.init
  , bullets = []
  , rocks =
    [ Rock.init 3 25 (fst rockVelocities) (fst rockPositions)
    , Rock.init 3 9 (snd rockVelocities) (snd rockPositions)
    ]
  , scoreboard = Scoreboard.init
  }


-- UPDATE

update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    bullets = Bullet.fire game.ship game.bullets

    collisionTests = List.map
      (detectCollisions game.rocks) bullets

    newBullets = bullets
      |> List.filterMap (Bullet.update dt)
      |> Bullet.removeDead (Bullet.onTarget collisionTests)

    newRocks = game.rocks
      |> List.map2 (Rock.update dt)
        (Rock.damaged (List.length game.rocks) collisionTests)
      |> List.concat

    newScoreboard = addRockScores collisionTests game.scoreboard
  in
  { game
  | ship = Ship.update (dt, keyInput, fireInput) game.ship
  , bullets = newBullets
  , rocks = newRocks
  , scoreboard = newScoreboard
  }


addRockScores : List (List (Physics.CollisionResult Rock))
  -> Scoreboard -> Scoreboard
addRockScores collisionTests board =
  let
    d = Debug.watch "col" collisionTests
    w = List.map fff collisionTests
    p = List.sum (List.concat w)
    d2 = Debug.watch "w" p
  in
  board

fff : List (Physics.CollisionResult Rock) -> List Int
fff =
  let
    w = List.map (\n -> (mySize n.object))
    d2 = Debug.watch "w2" w
  in
  w

mySize : Maybe (Physics.Collidable Rock) -> Int
mySize obj =
  let
    d = Debug.watch "d" obj
    q = case obj of
      Nothing ->
        0
      Just w ->
        w.size
  in
  q

detectCollisions : List (Physics.Collidable Rock) -> Bullet
  -> List (Physics.CollisionResult Rock)
detectCollisions targets bullet =
  List.map (Physics.collides bullet) targets


-- VIEW

view : Game -> Element
view game =
  let
    background = rect gameWidth gameHeight
      |> filled lightBlue
    allForms = List.concat
      [ [ background, Ship.draw game.ship ]
      , List.map Bullet.draw game.bullets
      , List.map Rock.draw game.rocks
      , Scoreboard.draw game.scoreboard
      ]
  in
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight allForms


-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update initGame inputSignal


inputSignal : Signal (Float, KeyInput, Bool)
inputSignal =
  let
    delta = fps 30
    tuples = Signal.map3 (,,) delta ui.steering ui.firing
  in
  Signal.sampleOn delta tuples


main : Signal Element
main =
  Signal.map view gameState

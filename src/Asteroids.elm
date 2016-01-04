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

-- MODEL

type alias Game =
  { ship : Ship
  , bullets: List Bullet
  , rocks : List Rock
  }


initGame : Game
initGame =
  { ship = initShip
  , bullets = []
  , rocks = [(initRock 50 32), (initRock -134 200)]
  }

-- UPDATE

update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    activeBullets = if game.ship.firing then
      (initBullet game.ship) :: game.bullets
    else
      game.bullets
  in
  { game
  | ship = updateShip (dt, keyInput, fireInput) game.ship
  , bullets = List.filterMap (updateBullet dt) activeBullets
  , rocks = List.map (updateRock dt) game.rocks
  }

-- VIEW

view : Game -> Element
view game =
  let
    background = rect gameWidth gameHeight
      |> filled lightBlue
    theShip = drawShip game.ship
    activeBullets = List.map drawBullet game.bullets
    activeRocks = List.map drawRock game.rocks
    allForms = List.concat
      [ [ background, theShip ]
      , activeBullets
      , activeRocks
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

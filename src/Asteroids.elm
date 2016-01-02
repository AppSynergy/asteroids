module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Ship exposing (..)
import Bullet exposing (..)
import Config exposing (..)

-- MODEL

-- http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Keyboard#arrows
type alias KeyInput =
  { x : Int
  , y : Int
  }


type alias Game =
  { ship : Ship
  , bullets: List Bullet
  }


initGame : Game
initGame =
  { ship = initShip
  , bullets = []
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
  , bullets = activeBullets
  }

-- VIEW

view : Game -> Element
view game =
  let
    activeBullets = List.map drawBullet game.bullets
  in
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      ([ rect gameWidth gameHeight
        |> filled black
      , drawShip game.ship
      ]
        |> List.append activeBullets )

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

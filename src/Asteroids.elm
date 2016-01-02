module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Keyboard
import Ship exposing (..)
import Generic exposing (..)

-- MODEL

type alias Game =
  { ship : Ship }


initGame : Game
initGame =
  { ship = initShip }

-- UPDATE

update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  { game
  | ship = updateShip (dt, keyInput, fireInput) game.ship
  }

-- VIEW

view : Game -> Element
view game =
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
        |> filled lightOrange
      , drawShip game.ship
      ]

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
  --Signal.map view gameState
  Signal.map show gameState

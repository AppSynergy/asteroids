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

update : (Float, KeyInput) -> Game -> Game
update (dt, keyInput) game =
  { game
  | ship = updateShip (dt, keyInput) game.ship
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


inputSignal : Signal (Float, KeyInput)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in Signal.sampleOn delta tuples


main : Signal Element
main =
  Signal.map view gameState
  --Signal.map show shipState

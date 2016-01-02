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


-- VIEW

view : Ship -> Element
view ship =
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
        |> filled lightOrange
      , drawShip ship
      ]


-- SIGNALS

shipState : Signal Ship
shipState =
  Signal.foldp updateShip initShip inputSignal


inputSignal : Signal (Float, KeyInput)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples


main : Signal Element
main =
  Signal.map view shipState
  --Signal.map show shipState

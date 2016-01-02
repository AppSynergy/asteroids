module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Keyboard

(gameWidth, gameHeight) = (620,480)

-- MODEL

type alias Vector2 =
  { x : Float
  , y : Float
  }

type alias Ship =
  { firing : Bool
  , velocity : Vector2
  , position : Vector2
  , facing : Float
  }

type Input
  = TurnLeft
  | TurnRight
  | Thrust
  | FireWeapon

type alias KeyInput =
  { x : Int
  , y : Int
  }

initShip : Ship
initShip =
  { firing = False
  , velocity = { x = 0, y = 0 }
  , position = { x = 0, y = 0 }
  , facing = 0
  }

-- UPDATE

update : (Float, KeyInput) -> Ship -> Ship
update (dt, keyInput) ship =
  let
    leftRightInput = toFloat keyInput.x
    upDownInput = toFloat keyInput.y
    -- only up arrow (thruster) does anything
    thrust = if upDownInput > 0 then upDownInput else 0
  in
  updateFacing leftRightInput ship

updateFacing : Float -> Ship -> Ship
updateFacing newFacing ship =
  let
    turnRate = 10
    dF = turnRate * newFacing + ship.facing
  in
  { ship | facing = dF }

updateThrust : Float -> Ship -> Ship
updateThrust thrust ship =
  ship --TODO


-- VIEW

-- SIGNALS

inputSignal : Signal (Float, KeyInput)
inputSignal =
  let delta = fps 30
      tuples = Signal.map2 (,) delta Keyboard.arrows
  in  Signal.sampleOn delta tuples


main : Signal Element
main =
  Signal.map show (Signal.foldp update initShip inputSignal)

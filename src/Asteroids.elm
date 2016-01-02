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
  , thrust : Float
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
  , thrust = 0
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
  -- apply the various update functions to Ship record
  ship
  |> updateFacing leftRightInput
  |> updateThrust thrust dt


updateFacing : Float -> Ship -> Ship
updateFacing newFacing ship =
  let
    turnRate = 10
    dF = turnRate * newFacing + ship.facing
  in
  { ship | facing = dF }


updateThrust : Float -> Float -> Ship -> Ship
updateThrust thrust dt ship =
  { ship
  | thrust = thrust
  , velocity = updateVelocity thrust ship.facing ship.velocity
  , position = updatePosition dt ship.velocity ship.position
  }


updateVelocity : Float -> Float -> Vector2 -> Vector2
updateVelocity thrust facing velocity =
  let
    thrustRate = 2
    facing' = degrees facing
  in
  if thrust == 1 then
    { velocity
    | y = velocity.y + thrustRate * cos facing' |> floor >> toFloat
    , x = velocity.x + thrustRate * sin facing' |> floor >> toFloat
    }
  else
    velocity

updatePosition : Float -> Vector2 -> Vector2 -> Vector2
updatePosition dt velocity position =
  let
    scale = 1
  in
  { position
  | y = position.y + velocity.y * scale * dt |> floor >> toFloat
  , x = position.x + velocity.x * scale * dt |> floor >> toFloat
  }

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

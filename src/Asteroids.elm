module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Keyboard

(gameWidth, gameHeight) = (620, 480)
(halfWidth, halfHeight) = (310, 240)

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


-- http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Keyboard#arrows
type alias KeyInput =
  { x : Int
  , y : Int
  }


type alias Game =
  { ship : Ship }


initShip : Ship
initShip =
  { firing = False
  , velocity = { x = 0, y = 0 }
  , position = { x = 0, y = 0 }
  , facing = 0
  , thrust = 0
  }


initGame : Game
initGame =
  { ship = initShip }


-- UPDATE

updateShip : (Float, KeyInput) -> Ship -> Ship
updateShip (dt, keyInput) ship =
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
    -- turn rate is -ve since left = -1
    turnRate = -10
    dF = turnRate * newFacing + ship.facing
  in
  { ship | facing = dF }


updateThrust : Float -> Float -> Ship -> Ship
updateThrust thrust dt ship =
  { ship
  | thrust = thrust
  , velocity = updateVelocity dt thrust ship.facing ship.velocity
  , position = updatePosition dt ship.velocity ship.position
  }


updateVelocity : Float -> Float -> Float -> Vector2 -> Vector2
updateVelocity dt thrust facing velocity =
  let
    thrustRate = 3
    facing' = degrees facing
    newVelocityY = velocity.y + thrustRate * cos facing'
    newVelocityX = velocity.x - thrustRate * sin facing'
  in
  if thrust == 1 then
    { velocity
    | y = newVelocityY |> floor >> toFloat
    , x = newVelocityX |> floor >> toFloat
    }
  else
    velocity |> updateDrag


wrapGeometry : Float -> Float -> Float
wrapGeometry position dimension =
  if position > dimension then
    -dimension
  else if position < -dimension then
    dimension
  else position


updatePosition : Float -> Vector2 -> Vector2 -> Vector2
updatePosition dt velocity position =
  let
    scale = 0.01
    newPositionY = position.y + velocity.y * scale * dt
    newPositionX = position.x + velocity.x * scale * dt
    newPositionY' = wrapGeometry newPositionY halfHeight
    newPositionX' = wrapGeometry newPositionX halfWidth
  in
  { position
  | y = newPositionY'
  , x = newPositionX'
  }


updateDrag : Vector2 -> Vector2
updateDrag velocity =
  let
    dragRate = 1
    dragRateX = if velocity.x > 0 then dragRate else -dragRate
    dragRateY = if velocity.y > 0 then dragRate else -dragRate
  in
  { velocity
  | y = if abs velocity.y > 0 then velocity.y - dragRateY else 0
  , x = if abs velocity.x > 0 then velocity.x - dragRateX else 0
  }

-- VIEW

view : Ship -> Element
view ship =
  container gameWidth gameHeight middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
        |> filled lightOrange
      , drawShip ship
      ]

drawShip : Ship -> Form
drawShip ship =
  let
    triangle = ngon 3 32
      |> filled green
    engines = rect 4 32
      |> filled black
      |> move ( -18 , 0 )
  in
  group [ triangle , engines ]
    |> rotate ( degrees (ship.facing + 90 ))
    |> move (ship.position.x, ship.position.y)

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

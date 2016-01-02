module Ship where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Generic exposing (..)

-- MODEL

type alias Ship =
  { firing : Bool
  , velocity : Vector2
  , position : Vector2
  , facing : Float
  , thrust : Float
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

updateShip : (Float, KeyInput, Bool) -> Ship -> Ship
updateShip (dt, keyInput, fireInput) ship =
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
  |> updateFiring fireInput


updateFiring : Bool -> Ship -> Ship
updateFiring fireInput ship =
  { ship | firing = fireInput }


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
    upperLimit = 100
    facing' = degrees facing
    newVelocityY = velocity.y + thrustRate * cos facing'
    newVelocityX = velocity.x - thrustRate * sin facing'
  in
  if thrust == 1 then
    { velocity
    | y = min upperLimit newVelocityY |> floor >> toFloat
    , x = min upperLimit newVelocityX |> floor >> toFloat
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
  | y = newPositionY' |> floor >> toFloat
  , x = newPositionX' |> floor >> toFloat
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

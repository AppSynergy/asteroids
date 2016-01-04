module Ship where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Config exposing (KeyInput)
import Physics exposing (..)

-- MODEL

type alias Ship =
  { firing : Bool
  , velocity : Vector2
  , position : Vector2
  , facing : Float
  , thrust : Float
  , coolDown : Int
  , coolDownTime : Int
  , turnRate : Float
  , thrustRate : Float
  , dragRate : Float
  , maximumSpeed : Float
  }


initShip : Ship
initShip =
  { firing = False
  , velocity = { x = 0, y = 0 }
  , position = { x = 0, y = 0 }
  , facing = 0
  , thrust = 0
  , coolDown = 0
  , coolDownTime = 8
  , turnRate = negate 10
  , thrustRate = 3
  , dragRate = 1
  , maximumSpeed = 100
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
  ship
  |> updateFacing leftRightInput
  |> updateThrust thrust dt
  |> updateFiring fireInput


updateFiring : Bool -> Ship -> Ship
updateFiring fireInput ship =
  let
    fireOK = ship.coolDown == 0 && fireInput
    coolDownTime = ship.coolDownTime
    newCoolDown = if fireOK then
      coolDownTime
    else if ship.coolDown > 0 then
      ship.coolDown - 1
    else
      ship.coolDown
  in
  { ship
  | firing = fireOK
  , coolDown = newCoolDown
  }


updateFacing : Float -> Ship -> Ship
updateFacing newFacing ship =
  let
    turnRate = ship.turnRate
    dF = turnRate * newFacing + ship.facing
  in
  { ship | facing = dF }


updateThrust : Float -> Float -> Ship -> Ship
updateThrust thrust dt ship =
  { ship
  | thrust = thrust
  , velocity = updateVelocity dt thrust ship
  , position = updatePosition True dt ship.velocity ship.position
  }


updateVelocity : Float -> Float -> Ship -> Vector2
updateVelocity dt thrust ship =
  let
    thrustRate = ship.thrustRate
    upperLimit = ship.maximumSpeed
    facing' = degrees ship.facing
    velocity = ship.velocity
    newVelocityY = velocity.y + thrustRate * cos facing'
    newVelocityX = velocity.x - thrustRate * sin facing'
  in
  if thrust == 1 then
    { velocity
    | y = min upperLimit newVelocityY |> floor >> toFloat
    , x = min upperLimit newVelocityX |> floor >> toFloat
    }
  else
    ship |> updateDrag


updateDrag : Ship -> Vector2
updateDrag ship =
  let
    dragRate = ship.dragRate
    velocity = ship.velocity
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
      |> filled lightGreen
    engines = rect 4 32
      |> filled lightOrange
      |> move (-18 , 0)
  in
  group [ triangle , engines ]
    |> rotate ( degrees (ship.facing + 90 ))
    |> move (ship.position.x, ship.position.y)

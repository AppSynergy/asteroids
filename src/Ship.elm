module Ship where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import UI exposing (KeyInput)
import Physics


-- MODEL

type alias Ship =
  { firing : Bool
  , velocity : Physics.Vector2
  , position : Physics.Vector2
  , facing : Float
  , thrust : Float
  , coolDown : Int
  , coolDownTime : Int
  , turnRate : Float
  , thrustRate : Float
  , dragRate : Float
  , maximumSpeed : Float
  , invulnerable : Bool
  , radius : Float
  , color1 : Color.Color
  , color2 : Color.Color
  }


init : Ship
init =
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
  , invulnerable = True
  , radius = 50
  , color1 = Color.lightGreen
  , color2 = Color.lightOrange
  }

-- UPDATE

update : (Float, KeyInput, Bool) -> Ship -> Ship
update (dt, keyInput, fireInput) ship =
  let
    leftRightInput = toFloat keyInput.x
    upDownInput = toFloat keyInput.y
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
  , position = Physics.updatePosition
    True dt ship.velocity ship.position
  }


updateVelocity : Float -> Float -> Ship -> Physics.Vector2
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


updateDrag : Ship -> Physics.Vector2
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

draw : Ship -> Form
draw ship =
  let
    triangle = ngon 3 32
      |> filled ship.color1
    engines = rect 4 32
      |> filled ship.color2
      |> move (-18 , 0)
  in
  group [ triangle , engines ]
    |> rotate ( degrees (ship.facing + 90 ))
    |> move (ship.position.x, ship.position.y)

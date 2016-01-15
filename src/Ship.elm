module Ship where

import Color
import Graphics.Collage as Draw

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
  , invulnerableCounter : Int
  , invulnerableTime : Int
  , radius : Float
  , color1 : Color.Color
  , color2 : Color.Color
  , dead : Bool
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
  , invulnerableCounter = 100
  , invulnerableTime = 100
  , radius = 28
  , color1 = Color.lightGreen
  , color2 = Color.lightOrange
  , dead = True
  }

-- UPDATE

update : (Float, KeyInput, Bool) -> Ship -> Ship
update (dt, keyInput, fireInput) ship =
  let
    leftRightInput = toFloat keyInput.x
    upDownInput = toFloat keyInput.y
    thrust = if upDownInput > 0 then upDownInput else 0
  in
  if ship.dead then ship
  else ship
    |> updateFacing leftRightInput
    |> updateThrust thrust dt
    |> updateFiring fireInput
    |> updateInvulnerable


updateInvulnerable : Ship -> Ship
updateInvulnerable ship =
  let
    newTime = if ship.invulnerable && ship.invulnerableCounter > 0 then
      ship.invulnerableCounter - 1
    else
      ship.invulnerableCounter
  in
  { ship
  | invulnerable = ship.invulnerableCounter > 1
  , invulnerableCounter = newTime
  }

updateFiring : Bool -> Ship -> Ship
updateFiring fireInput ship =
  let
    fireOK = ship.coolDown == 0 && fireInput
    newCoolDown = if fireOK then
      ship.coolDownTime
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


loseLife : Bool -> Ship -> Ship
loseLife hit ship =
  if hit then
    { ship
    | position = { x = 0, y = 0 }
    , invulnerable = True
    , invulnerableCounter = ship.invulnerableTime
    }
  else
    ship


-- VIEW

draw : Ship -> Draw.Form
draw ship =
  let
    itime = ship.invulnerableCounter
    flashColor = if itime > 0 then
      if itime % 5 == 0 then Color.white else ship.color1
    else
      ship.color1
    triangle = Draw.ngon 3 32
      |> Draw.filled flashColor
    engines = Draw.rect 4 32
      |> Draw.filled ship.color2
      |> Draw.move (-18 , 0)
    dline = Draw.defaultLine
    bound = Draw.circle ship.radius
      |> Draw.outlined { dline | color = Color.red }
  in
  if ship.dead then
    Draw.circle 0
      |> Draw.filled Color.black
  else
    Draw.group [ triangle , engines , bound ]
      |> Draw.rotate ( degrees (ship.facing + 90 ))
      |> Draw.move (ship.position.x, ship.position.y)

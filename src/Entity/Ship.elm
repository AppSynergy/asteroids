module Entity.Ship where

import Color
import Graphics.Collage as Draw

import UI exposing (KeyInput)
import Physics


-- MODEL

type alias Ship =
  Physics.Cooldownable ( Physics.Collidable
  { velocity : Physics.Vector2
  , facing : Float
  , thrust : Float
  , turnRate : Float
  , thrustRate : Float
  , dragRate : Float
  , maximumSpeed : Float
  , invulnerable : Bool
  , invulnerableCounter : Int
  , invulnerableTime : Int
  , color1 : Color.Color
  , color2 : Color.Color
  })


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
  }

-- UPDATE

update : (Float, KeyInput, Bool) -> Ship -> Ship
update (dt, keyInput, fireInput) ship =
  let
    upDownInput = toFloat keyInput.y
    thrust = if upDownInput > 0 then upDownInput else 0
  in
  ship
    |> (updateFacing << toFloat) keyInput.x
    |> updateThrust thrust dt
    |> Physics.cooldown fireInput
    |> updateInvulnerable


updateInvulnerable : Ship -> Ship
updateInvulnerable ship =
  let
    newTime =
      if ship.invulnerable && ship.invulnerableCounter > 0 then
        ship.invulnerableCounter - 1
      else
        ship.invulnerableCounter
  in
  { ship
  | invulnerable = ship.invulnerableCounter > 1
  , invulnerableCounter = newTime
  }


updateFacing : Float -> Ship -> Ship
updateFacing newFacing ship =
  { ship | facing =
    ship.turnRate * newFacing + ship.facing
  }


updateThrust : Float -> Float -> Ship -> Ship
updateThrust thrust dt ship =
  { ship
  | thrust = thrust
  , velocity = updateVelocity dt thrust ship
  , position = ship.position
    |> Physics.updatePosition True dt ship.velocity
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
  if not ship.invulnerable && hit then
    { ship
    | position = { x = 0, y = 0 }
    , invulnerable = True
    , invulnerableCounter = ship.invulnerableTime
    }
  else
    ship

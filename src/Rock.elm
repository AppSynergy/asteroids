module Rock where

import Color
import List.Extra exposing (transpose)

import Physics


-- MODEL

type alias Rock =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , size : Int
  , radius : Float
  , color1 : Color.Color
  , color2 : Color.Color
  , facing : Float
  , spinRate : Float
  }


init : Int -> Float -> Physics.Vector2 -> Physics.Vector2 -> Rock
init size spin velocity position =
  { velocity = velocity
  , position = position
  , size = size
  , radius = toFloat (8 * size)
  , color1 = Color.rgb 170 170 170
  , color2 = Color.rgb 100 100 100
  , facing = 0
  , spinRate = spin
  }


getSize : Maybe Rock -> Int
getSize rock =
  case rock of
    Nothing ->
      0
    Just rock ->
      rock.size


-- UPDATE

update : Float -> Bool -> Rock -> List Rock
update dt damage rock =
  let
    newRock = split damage rock
  in
  List.map (update' dt) newRock


update' : Float -> Rock -> Rock
update' dt rock =
  let
    spinningRock = updateFacing dt rock
  in
  { spinningRock
  | position = Physics.updatePosition
    True dt rock.velocity rock.position
  }


updateFacing : Float -> Rock -> Rock
updateFacing dt rock =
  { rock
  | facing = rock.facing + (dt / 10)
  }


damaged : Int -> Physics.CollisionMatrix a -> List Bool
damaged rockCount collisionTests =
  let
    ct = List.map Physics.hitAny (transpose collisionTests)
  in
  if List.length ct < 1 then
    List.repeat rockCount False
  else ct


split : Bool -> Rock -> List Rock
split damage rock =
  let
    (v1,v2) = scatterVelocities rock.velocity
  in
  if damage && rock.size == 1 then []
  else if damage then
    [ init (rock.size - 1) rock.spinRate v1 rock.position
    , init (rock.size - 1) rock.spinRate v2 rock.position
    ]
  else
    [ rock ]


scatterVelocities : Physics.Vector2 -> (Physics.Vector2, Physics.Vector2)
scatterVelocities velocity =
  let
    accel = 1.1
    scatter = degrees 10
    angle = atan2 velocity.y velocity.x
    angle1 = (angle - scatter)
    angle2 = (angle + scatter)
    speed = Physics.toScalar velocity
  in
  ( { x = speed * accel * (cos angle1)
    , y = speed * accel * (sin angle1)
    }
  , { x = speed * accel * (cos angle2)
    , y = speed * accel * (sin angle2)
    }
  )

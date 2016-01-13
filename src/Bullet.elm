module Bullet where

import Color
import Graphics.Collage as Draw

import Ship exposing (Ship)
import Physics


-- MODEL

type alias Bullet =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , lifetime : Int
  , radius: Float
  , speed : Float
  , color : Color.Color
  }


init : Ship -> Bullet
init ship =
  let
    speed = 50
  in
  { velocity = firingVelocity ship.facing speed
  , position = ship.position
  , lifetime = 50
  , radius = 5
  , speed = speed
  , color = Color.lightRed
  }


firingVelocity : Float -> Float -> Physics.Vector2
firingVelocity facing speed =
  let
    facing' = degrees facing
  in
  { y = speed * cos facing'
  , x = speed * negate (sin facing')
  }


-- UPDATE

update : Float -> Bullet -> Maybe Bullet
update dt bullet =
  let
    stillAlive = bullet.lifetime > 0
    aging = if stillAlive then bullet.lifetime - 1 else 0
    newPosition = Physics.updatePosition
      False dt bullet.velocity bullet.position
  in
  if stillAlive then
    Just { bullet
    | position = newPosition
    , lifetime = aging
    }
  else
    Nothing


fire : Ship -> List Bullet -> List Bullet
fire ship bullets =
  if ship.firing then
    (init ship) :: bullets
  else
    bullets


removeDead : List Bool -> List Bullet -> List Bullet
removeDead hits bullets =
  let
    zipped = List.map2 (,) hits bullets
    rmv a = if (fst a) then Nothing else Just (snd a)
  in
  List.filterMap rmv zipped


onTarget : List (List (Physics.CollisionResult a)) -> List Bool
onTarget collisionTests =
  List.map Physics.hitAny collisionTests


-- VIEW

draw : Bullet -> Draw.Form
draw bullet =
  Draw.circle bullet.radius
    |> Draw.filled bullet.color
    |> Draw.move (bullet.position.x, bullet.position.y)

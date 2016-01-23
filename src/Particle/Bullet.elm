module Particle.Bullet where

import Color
import Graphics.Collage as Draw

import Entity.Ship as Ship exposing (Ship)
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
  { velocity = Physics.toVector speed ship.facing
  , position = ship.position
  , lifetime = 50
  , radius = 5
  , speed = speed
  , color = Color.lightRed
  }


-- UPDATE

update : Float -> Bullet -> Maybe Bullet
update dt bullet =
  let
    newPosition = Physics.updatePosition
      False dt bullet.velocity bullet.position
    newBullet = { bullet | position = newPosition }
  in
  Physics.expiration newBullet


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


onTarget : Physics.CollisionMatrix a -> List Bool
onTarget collisionTests =
  List.map Physics.hitAny collisionTests

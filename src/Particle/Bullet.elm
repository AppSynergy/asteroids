module Particle.Bullet where

import Color

import Entity.Ship exposing (Ship)
import Physics


-- MODEL

type alias CanShoot a =
  { a
  | facing : Float
  , position : Physics.Vector2
  , firing : Bool
  }


type alias Bullet =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , lifetime : Int
  , radius: Float
  , speed : Float
  , color : Color.Color
  }


init : CanShoot a -> Bullet
init shooter =
  let
    speed = 50
  in
  { velocity = Physics.toVector speed shooter.facing
  , position = shooter.position
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


fire : CanShoot a -> List Bullet -> List Bullet
fire shooter bullets =
  if shooter.firing then
    (init shooter) :: bullets
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

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
  , bulletType : BulletType
  }


init : CanShoot a -> BulletType -> Bullet
init shooter bulletType =
  { velocity = Physics.toVector bulletType.speed shooter.facing
  , position = shooter.position
  , lifetime = bulletType.lifetime
  , radius = bulletType.radius
  , bulletType = bulletType
  }


type alias BulletType =
  { speed : Float
  , lifetime : Int
  , radius : Float
  , color : Color.Color
  }


initDefaultBullet : BulletType
initDefaultBullet =
  { speed = 45
  , lifetime = 60
  , radius = 4
  , color = Color.lightGreen
  }


initSaucerBullet : BulletType
initSaucerBullet =
  { speed = 25
  , lifetime = 150
  , radius = 6
  , color = Color.lightPurple
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


fire : CanShoot a -> String -> List Bullet -> List Bullet
fire shooter bulletType bullets =
  if shooter.firing then
    case bulletType of
      "saucer" -> (init shooter initSaucerBullet) :: bullets
      _ -> (init shooter initDefaultBullet) :: bullets
  else
    bullets


removeDead : List Bool -> List Bullet -> List Bullet
removeDead hits bullets =
  let
    zipped = List.map2 (,) hits bullets
    rmv a = if (fst a) then Nothing else Just (snd a)
  in
  List.filterMap rmv zipped

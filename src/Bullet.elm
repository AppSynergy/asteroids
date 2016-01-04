module Bullet where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Ship exposing (Ship)
import Physics exposing (..)

-- MODEL

type alias Bullet =
  { velocity : Vector2
  , position : Vector2
  , lifetime : Int
  }


initBullet : Ship -> Bullet
initBullet ship =
  { velocity = firingVelocity ship.facing
  , position = ship.position
  , lifetime = 50
  }


firingVelocity : Float -> Vector2
firingVelocity facing =
  let
    bulletSpeed = 50
    facing' = degrees facing
  in
  { y = bulletSpeed * cos facing'
  , x = bulletSpeed * negate (sin facing')
  }

-- UPDATE

updateBullet : Float -> Bullet -> Maybe Bullet
updateBullet dt bullet =
  let
    stillAlive = bullet.lifetime > 0
    aging = if stillAlive then bullet.lifetime - 1 else 0
  in
  if stillAlive then
    Just { bullet
    | position = updatePosition False dt bullet.velocity bullet.position
    , lifetime = aging
    }
  else
    Nothing

-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 5
    |> filled lightRed
    |> move (bullet.position.x, bullet.position.y)

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
  }


initBullet : Ship -> Bullet
initBullet ship =
  { velocity = firingVelocity ship
  , position = ship.position
  }


firingVelocity : Ship -> Vector2
firingVelocity ship =
  let
    bulletSpeed = 50
    facing = degrees ship.facing
  in
  { y = bulletSpeed * cos facing
  , x = bulletSpeed * negate (sin facing)
  }

-- UPDATE

updateBullet : Float -> Bullet -> Bullet
updateBullet dt bullet =
  { bullet
  | position = updatePosition False dt bullet.velocity bullet.position
  }

-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 5
    |> filled lightRed
    |> move (bullet.position.x, bullet.position.y)

module Bullet where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Generic exposing (..)
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


-- currently the bullet travels at twice the ship speed
-- this is simple, but not correct
firingVelocity : Ship -> Vector2
firingVelocity ship =
  let
    velocity = ship.velocity
  in
  { velocity
  | y = velocity.y * 2
  , x = velocity.x * 2
  }


-- UPDATE

-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 5
    |> filled lightRed

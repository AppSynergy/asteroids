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

firingVelocity : Ship -> Vector2
firingVelocity ship =
  ship.velocity --todo: bullet must go faster than ship!


-- UPDATE

-- VIEW

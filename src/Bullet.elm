module Bullet where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Generic exposing (..)
import Ship exposing (Ship)

-- MODEL

type alias Bullet =
  { velocity : Vector2
  , position : Vector2
  }

initBullet : Ship -> Bullet
initBullet ship =
  { velocity = firingVelocity ship.velocity
  , position = ship.position
  }

firingVelocity : Vector2 -> Vector2
firingVelocity velocity =
  velocity --todo: bullet must go faster than ship!

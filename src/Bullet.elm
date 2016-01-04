module Bullet where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Ship exposing (Ship)
import Rock exposing (Rock)
import Physics

-- MODEL

type alias Bullet =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , lifetime : Int
  }


initBullet : Ship -> Bullet
initBullet ship =
  { velocity = firingVelocity ship.facing
  , position = ship.position
  , lifetime = 50
  }


firingVelocity : Float -> Physics.Vector2
firingVelocity facing =
  let
    bulletSpeed = 50
    facing' = degrees facing
  in
  { y = bulletSpeed * cos facing'
  , x = bulletSpeed * negate (sin facing')
  }

-- UPDATE

updateBullet : Float -> List Rock -> Bullet -> Maybe Bullet
updateBullet dt targets bullet =
  let
    stillAlive = bullet.lifetime > 0
    aging = if stillAlive then bullet.lifetime - 1 else 0
    newPosition = Physics.updatePosition
      False dt bullet.velocity bullet.position
    hitTarget = detectCollisions targets bullet
  in
  if stillAlive && not hitTarget then
    Just { bullet
    | position = newPosition
    , lifetime = aging
    }
  else
    Nothing


detectCollisions : List Rock -> Bullet -> Bool
detectCollisions rocks bullet =
  let
    collisions = List.map (Physics.collides bullet) rocks
  in
  List.foldr (||) False collisions

-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle 5
    |> filled lightRed
    |> move (bullet.position.x, bullet.position.y)

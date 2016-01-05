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
  , radius: Float
  , speed : Float
  }


initBullet : Ship -> Bullet
initBullet ship =
  let
    speed = 50
  in
  { velocity = firingVelocity ship.facing speed
  , position = ship.position
  , lifetime = 50
  , radius = 5
  , speed = speed
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

updateBullet : Float -> Bullet
  -> Maybe Bullet
updateBullet dt bullet =
  let
    stillAlive = bullet.lifetime > 0
    aging = if stillAlive then bullet.lifetime - 1 else 0
    newPosition = Physics.updatePosition
      False dt bullet.velocity bullet.position
    --hits = detectCollisions targets bullet
    --hitSomething = List.any (\n -> n.result == True) hits
    --doHits = recordHit hits
  in
  if stillAlive then --&& not hitSomething then
    Just { bullet
    | position = newPosition
    , lifetime = aging
    }
  else
    Nothing


-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle bullet.radius
    |> filled lightRed
    |> move (bullet.position.x, bullet.position.y)

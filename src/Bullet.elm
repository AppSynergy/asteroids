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

updateBullet : Float -> List (Physics.Collidable a) -> Bullet
  -> Maybe Bullet
updateBullet dt targets bullet =
  let
    stillAlive = bullet.lifetime > 0
    aging = if stillAlive then bullet.lifetime - 1 else 0
    newPosition = Physics.updatePosition
      False dt bullet.velocity bullet.position
    (hitHappened, hitWho) = detectCollisions targets bullet
    dbg = Debug.watch "hit" hitWho
  in
  if stillAlive && not hitHappened then
    Just { bullet
    | position = newPosition
    , lifetime = aging
    }
  else
    Nothing


detectCollisions : List (Physics.Collidable a) -> Bullet
  -> (Bool, Maybe (Physics.Collidable a))
detectCollisions targets bullet =
  let
    collisions = List.map (Physics.collides bullet) targets
    hitBools = List.foldr (||) False
      (List.map (fst) collisions)
    hitObjects = (List.map (snd) collisions)
  in
   (hitBools, firstNonEmpty hitObjects)


firstNonEmpty : List (Maybe a) -> Maybe a
firstNonEmpty list =
  List.head (List.filterMap identity list)


-- VIEW

drawBullet : Bullet -> Form
drawBullet bullet =
  circle bullet.radius
    |> filled lightRed
    |> move (bullet.position.x, bullet.position.y)

module Physics where

import Config exposing (halfWidth, halfHeight)
import Random exposing (..)

type alias Vector2 =
  { x : Float
  , y : Float
  }


positionSeed : Seed
positionSeed = Random.initialSeed 46756


randomY : Generator Float
randomY =
  Random.float (negate halfHeight) halfHeight


randomX : Generator Float
randomX =
  Random.float (negate halfWidth) halfWidth


randomPosition :  Vector2
randomPosition =
  let
    rY = Random.generate randomY positionSeed
    rX = Random.generate randomX positionSeed
  in
    { x = (fst rX), y = (fst rY) }


updatePosition : Bool -> Float -> Vector2 -> Vector2 -> Vector2
updatePosition isWrappable dt velocity position =
  let
    scale = 0.01
    newPositionY = position.y + velocity.y * scale * dt
    newPositionX = position.x + velocity.x * scale * dt
    newPositionY' = if isWrappable then
      wrapGeometry newPositionY halfHeight
    else
      newPositionY
    newPositionX' = if isWrappable then
      wrapGeometry newPositionX halfWidth
    else
      newPositionX
  in
  { position
  | y = newPositionY' |> floor >> toFloat
  , x = newPositionX' |> floor >> toFloat
  }


wrapGeometry : Float -> Float -> Float
wrapGeometry position dimension =
  if position > dimension then
    -dimension
  else if position < -dimension then
    dimension
  else position

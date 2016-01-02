module Physics where

import Generic exposing (..)

updatePosition : Float -> Vector2 -> Vector2 -> Vector2
updatePosition dt velocity position =
  let
    scale = 0.01
    newPositionY = position.y + velocity.y * scale * dt
    newPositionX = position.x + velocity.x * scale * dt
    newPositionY' = wrapGeometry newPositionY halfHeight
    newPositionX' = wrapGeometry newPositionX halfWidth
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

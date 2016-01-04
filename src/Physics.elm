module Physics where

import Config exposing (halfWidth, halfHeight)

-- MODEL

type alias Vector2 =
  { x : Float
  , y : Float
  }


type alias Positioned a =
  { a | position : Vector2 }

-- UPDATE

near : Float -> Float -> Float -> Bool
near k c n =
  n >= k - c && n <= k + c


collides : Positioned a -> Positioned b -> Bool
collides obj1 obj2 =
  let
    range = 25
  in
  near obj1.position.x range obj2.position.x
    && near obj1.position.y range obj2.position.y


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

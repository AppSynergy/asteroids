module Physics where

import Config exposing (halfWidth, halfHeight)

-- MODEL

type alias Vector2 =
  { x : Float
  , y : Float
  }


type alias Collidable a =
  { a
  | position : Vector2
  , radius: Float
  }

-- UPDATE

near : Float -> Float -> Float -> Bool
near k c n =
  n >= k - c && n <= k + c


collides : Collidable a -> Collidable b
  -> (Bool, Maybe (Collidable b))
collides obj1 obj2 =
  let
    check = near obj1.position.x obj2.radius obj2.position.x
      && near obj1.position.y obj2.radius obj2.position.y
    hitObject = if check then
      Just obj2
    else
      Nothing
  in
    (check, hitObject)


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

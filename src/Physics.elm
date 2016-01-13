module Physics where

import UI exposing (halfWidth, halfHeight)


-- MODEL

type alias Vector2 =
  { x : Float
  , y : Float
  }


type alias Expirable a =
  { a
  | lifetime : Int
  }


type alias Collidable a =
  { a
  | position : Vector2
  , radius : Float
  }


type alias CollisionResult a =
  { result : Bool
  , object : Maybe (Collidable a)
  }


toScalar : Vector2 -> Float
toScalar vector =
  sqrt ((vector.y ^ 2) + (vector.x ^ 2))


toVector : Float -> Float -> Vector2
toVector magnitude angle =
  let
    angle' = degrees angle
  in
  { y = (magnitude * cos angle')
  , x = (magnitude * negate (sin angle'))
  }


-- UPDATE

expiration : Expirable a -> Maybe (Expirable a)
expiration obj =
  let
    stillAlive = obj.lifetime > 0
    aging = if stillAlive then obj.lifetime - 1 else 0
  in
  if stillAlive then
    Just { obj | lifetime = aging }
  else
    Nothing


hitAny : List (CollisionResult a) -> Bool
hitAny =
  List.any (\n -> n.result == True)


near : Float -> Float -> Float -> Bool
near k c n =
  n >= k - c && n <= k + c


collides : Collidable a -> Collidable b -> CollisionResult b
collides obj1 obj2 =
  let
    check = near obj1.position.x obj2.radius obj2.position.x
      && near obj1.position.y obj2.radius obj2.position.y
    hit = if check then
      Just obj2
    else
      Nothing
  in
    { result = check, object = hit }


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

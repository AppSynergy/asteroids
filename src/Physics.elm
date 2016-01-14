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


type alias CollisionMatrix a = List (List (CollisionResult a))


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


collides : Collidable a -> Collidable b -> CollisionResult b
collides obj1 obj2 =
  let
    impactParam = obj1.radius + obj2.radius
    bx = abs ( obj1.position.x - obj2.position.x )
    by = abs ( obj1.position.y - obj2.position.y )
    b = toScalar { x = bx, y = by }
    check = b < impactParam
    hit = if check then
      Just obj2
    else
      Nothing
  in
    { result = check, object = hit }


getCollidePositions : List (CollisionResult a) -> List Vector2
getCollidePositions collisions =
  let
    positions b = case b of
      Just b -> Just b.position
      Nothing -> Nothing
  in
  List.filterMap (\a -> positions a.object) collisions


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

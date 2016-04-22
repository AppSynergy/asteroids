module Entity.Destroyable where

import List.Extra exposing (transpose)
import Physics


damaged : Int -> Physics.CollisionMatrix a -> List Bool
damaged objectCount collisionTests =
  let
    ct = collisionTests
      |> transpose
      |> List.map Physics.hitAny
  in
  if List.length ct < 1 then
    List.repeat objectCount False
  else ct

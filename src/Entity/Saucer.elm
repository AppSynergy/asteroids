module Entity.Saucer where

import Color
import List.Extra exposing (transpose)

import Physics


-- MODEL

type alias Saucer =
  Physics.Cooldownable ( Physics.Collidable
  { velocity : Physics.Vector2
  , size : Int
  , skill : Int
  , facing : Float
  })


init : Int -> Physics.Vector2 -> Saucer
init skill position =
  { velocity = { x = 5, y = 5}
  , position = position
  , size = 1
  , radius = 40
  , skill = skill
  , facing = 0
  , firing = True
  , coolDown = 0
  , coolDownTime = 50
  }


-- UPDATE

update : Float -> Physics.Collidable a -> Saucer -> Bool -> List Saucer
update dt target saucer damage =
  if damage then []
  else
    { saucer
    | position = saucer.position
      |> Physics.updatePosition True dt saucer.velocity
    }
      |> updateFacing 6 target
      |> Physics.cooldown True
      |> (\x -> [x])


updateFacing : Float -> Physics.Collidable a -> Saucer -> Saucer
updateFacing newFacing target saucer =
  let
    t = Physics.diff saucer.position target.position
    d = atan2 t.y t.x
    d1 = Debug.watch "fd" (d,t.x,t.y)
  in
  { saucer
  | facing = (d * 180 / pi) + 90
  }


damaged : Int -> Physics.CollisionMatrix a -> List Bool
damaged sCount collisionTests =
  let
    ct = collisionTests
      |> transpose
      |> List.map Physics.hitAny
  in
  if List.length ct < 1 then
    List.repeat sCount False
  else ct

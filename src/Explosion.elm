module Explosion where

import Color
import Graphics.Collage as Draw

import Physics


-- MODEL

type alias Explosion =
  { position : Physics.Vector2
  , fragments : List Fragment
  , color: Color.Color
  , lifetime : Int
  }


type alias Fragment =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  }


init : Physics.Vector2 -> Explosion
init position =
  let
    angles = List.scanl (+) 0 (List.repeat 9 36)
    velocities = List.map (Physics.toVector 16) angles
  in
  { position = position
  , fragments = List.map (initFragment position) velocities
  , color = Color.yellow
  , lifetime = 20
  }


initFragment : Physics.Vector2 -> Physics.Vector2 -> Fragment
initFragment position velocity =
  { velocity = velocity
  , position = position
  }


-- UPDATE

update : Float -> Explosion -> Maybe Explosion
update dt explosion =
  let
    stillAlive = explosion.lifetime > 0
    aging = if stillAlive then explosion.lifetime - 1 else 0
  in
  if stillAlive then
    Just { explosion
    | fragments = List.map (updateFragment dt) explosion.fragments
    , lifetime = aging
    }
  else
    Nothing


updateFragment : Float -> Fragment -> Fragment
updateFragment dt fragment =
  { fragment
  | position = Physics.updatePosition
    False dt fragment.velocity fragment.position
  }


-- VIEW

draw : Explosion -> Draw.Form
draw explosion =
  explosion.fragments
    |> List.map (drawFragment explosion.color)
    |> Draw.group


drawFragment : Color.Color -> Fragment -> Draw.Form
drawFragment color fragment =
  Draw.circle 3
    |> Draw.filled color
    |> Draw.move (fragment.position.x, fragment.position.y)

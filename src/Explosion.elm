module Explosion where

import Color
import Graphics.Collage as Draw

import Physics


-- MODEL

type alias Explosion =
  { position : Physics.Vector2
  , fragments : List Fragment
  , color: Color.Color
  }


type alias Fragment =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , lifetime : Int
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
  }


initFragment : Physics.Vector2 -> Physics.Vector2 -> Fragment
initFragment position velocity =
  { velocity = velocity
  , position = position
  , lifetime = 20
  }


-- UPDATE

update : Float -> Explosion -> Explosion
update dt explosion =
  { explosion
  | fragments = List.map (updateFragment dt) explosion.fragments
  }


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

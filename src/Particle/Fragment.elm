module Particle.Fragment where

import Color
import Graphics.Collage as Draw

import Physics


-- MODEL

type alias Fragment =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  }


init : Physics.Vector2 -> Physics.Vector2 -> Fragment
init position velocity =
  { velocity = velocity
  , position = position
  }


-- UPDATE

update : Float -> Fragment -> Fragment
update dt fragment =
  { fragment
  | position = Physics.updatePosition
    False dt fragment.velocity fragment.position
  }


-- VIEW

draw : Color.Color -> Fragment -> Draw.Form
draw color fragment =
  Draw.circle 3
    |> Draw.filled color
    |> Draw.move (fragment.position.x, fragment.position.y)

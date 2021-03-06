module Particle.Fragment where

import Color

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

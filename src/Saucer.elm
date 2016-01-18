module Saucer where

import Color
import Graphics.Collage as Draw

import Physics


-- MODEL

type alias Saucer =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , size : Int
  , radius : Float
  , skill : Int
  }


init : Int -> Physics.Vector2 -> Saucer
init skill position =
  { velocity = { x = 0, y = 0}
  , position = position
  , size = 1
  , radius = 24
  , skill = skill
  }

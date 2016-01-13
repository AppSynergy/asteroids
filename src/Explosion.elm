module Explosion where

import Color

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
  }

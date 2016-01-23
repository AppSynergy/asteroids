module Entity.Saucer where

import Color

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
  { velocity = { x = 5, y = 5}
  , position = position
  , size = 1
  , radius = 24
  , skill = skill
  }


-- UPDATE

update : Float -> Saucer -> Saucer
update dt saucer =
  { saucer
  | position = Physics.updatePosition True dt saucer.velocity saucer.position
  }

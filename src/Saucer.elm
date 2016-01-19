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
  { velocity = { x = 1, y = 1}
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


-- VIEW

draw : Saucer -> Draw.Form
draw saucer =
  let d = Debug.watch "saucer" saucer.position.x in
  Draw.circle saucer.radius
    |> Draw.filled Color.darkRed
    |> Draw.move (saucer.position.x, saucer.position.y)

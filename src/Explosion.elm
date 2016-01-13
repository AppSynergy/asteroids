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
  { position = position
  , fragments = List.repeat 10 (initFragment position)
  , color = Color.yellow
  }


initFragment : Physics.Vector2 -> Fragment
initFragment position =
  { velocity = { x = 2, y = 2 }
  , position = position
  , lifetime = 20
  }


-- UPDATE

update : Float -> Explosion -> Explosion
update dt explosion =
  explosion


-- VIEW

draw : Explosion -> Draw.Form
draw explosion =
  let
    db = Debug.watch "expl" explosion
  in
  Draw.circle 5
    |> Draw.filled explosion.color
    |> Draw.move (explosion.position.x, explosion.position.y)

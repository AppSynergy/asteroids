module Skins.Basic.Sprites where

import Color
import Graphics.Collage as Draw

import Physics
import Rock

type alias Spritable a =
  { a
  | position : Physics.Vector2
  , radius : Float
  , facing : Float
  , color1 : Color.Color
  , color2 : Color.Color
  }

--type Spritable2 = Rock | AllTheOthers

sprite : Spritable2 -> Draw.Form
sprite actor =
  case actor of

    _ ->
      "error"

    Rock ->
      let
        body = Draw.circle actor.radius
          |> Draw.filled actor.color1
        spot1 = Draw.circle (actor.radius / 5)
          |> Draw.filled actor.color2
          |> Draw.move (actor.radius / 3 , actor.radius / 2)
        spot2 = Draw.circle (actor.radius / 4)
          |> Draw.filled actor.color2
          |> Draw.move (actor.radius / -2 , actor.radius / 3.5)
        spot3 = Draw.circle (actor.radius / 7)
          |> Draw.filled actor.color2
          |> Draw.move (actor.radius / -3 , actor.radius / -1.6)
      in
      Draw.group [body, spot1, spot2, spot3]
        |> Draw.rotate (degrees actor.facing)
        |> Draw.move (actor.position.x, actor.position.y)

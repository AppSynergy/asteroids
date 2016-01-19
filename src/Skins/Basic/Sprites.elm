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


positionForm : List Draw.Form -> Spritable a -> Draw.Form
positionForm components actor =
  Draw.group components
    |> Draw.rotate (degrees actor.facing + 90)
    |> Draw.move (actor.position.x, actor.position.y)


sprite : String -> Spritable a -> Draw.Form
sprite label actor =
  case label of

    "Bullet" ->
      let
        dot = Draw.circle actor.radius
          |> Draw.filled actor.color1
      in
      positionForm [dot] actor

    "Rock" ->
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
      positionForm [body, spot1, spot2, spot3] actor

    _ ->
      Debug.crash "not implemented"

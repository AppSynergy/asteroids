module Scoreboard where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Text

import Physics
import Rock exposing (Rock)


-- MODEL

type alias Scoreboard =
  { score : Int
  , lives : Int
  }


init : Scoreboard
init =
  { score = 0
  , lives = 3
  }


-- UPDATE

update : Physics.CollisionMatrix Rock -> Scoreboard -> Scoreboard
update collisionTests board =
  let
    collToObj = \n -> Rock.getSize n.object
    sizeHitPerBullet = List.map List.sum
      (List.map (List.map collToObj) collisionTests)
    newScores = List.map (\n -> 60//n) sizeHitPerBullet
  in
  { board
  | score = board.score + List.sum newScores
  }


-- VIEW

draw : Scoreboard -> List Draw.Form
draw board =
  let
    scoreText =
      "SCORE : " ++ (toString board.score)
        |> Text.fromString >> Element.leftAligned >> Draw.toForm
        |> Draw.move (200,200)
    livesText =
      "LIVES : " ++ (toString board.lives)
        |> Text.fromString >> Element.leftAligned >> Draw.toForm
        |> Draw.move (200,170)
  in
  [scoreText, livesText]

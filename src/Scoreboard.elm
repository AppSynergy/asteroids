module Scoreboard where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
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

update : List (List (Physics.CollisionResult Rock)) -> Scoreboard -> Scoreboard
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

draw : Scoreboard -> List Form
draw board =
  let
    scoreText =
      "SCORE : " ++ (toString board.score)
        |> Text.fromString >> leftAligned >> toForm
        |> move (200,200)
    livesText =
      "LIVES : " ++ (toString board.lives)
        |> Text.fromString >> leftAligned >> toForm
        |> move (200,170)
  in
  [scoreText, livesText]

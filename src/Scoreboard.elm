module Scoreboard where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Text


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

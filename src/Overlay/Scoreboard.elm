module Overlay.Scoreboard where

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


scoreboardStyle =
  { typeface = [ "Times New Roman", "serif" ]
  , height = Just 20
  , color = Color.white
  , bold = True
  , italic = False
  , line = Nothing
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
        |> textFormat
        |> Draw.move (200,200)
    livesText =
      "LIVES : " ++ (toString board.lives)
        |> textFormat
        |> Draw.move (200,170)
  in
  [scoreText, livesText]


textFormat : String -> Draw.Form
textFormat string =
  string
    |> Text.fromString
    |> Text.style scoreboardStyle
    >> Element.rightAligned
    |> Element.width 100
    >> Draw.toForm

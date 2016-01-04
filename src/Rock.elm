module Rock where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Config exposing (KeyInput)
import Physics

-- MODEL

type alias Rock =
  { velocity : Physics.Vector2
  , position : Physics.Vector2
  , size : Int
  }


initRock : Float -> Float -> Rock
initRock a b =
  { velocity = { x = b / 10, y = a / 10 }
  , position = { x = a, y = b }
  , size = 3
  }

-- UPDATE

updateRock : Float -> Rock -> Rock
updateRock dt rock =
  { rock
  | position = Physics.updatePosition True dt rock.velocity rock.position
  }

-- VIEW

drawRock : Rock -> Form
drawRock rock =
  circle 20
    |> filled blue
    |> move (rock.position.x, rock.position.y)

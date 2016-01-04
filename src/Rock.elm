module Rock where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Config exposing (KeyInput)
import Physics exposing (..)

-- MODEL

type alias Rock =
  { velocity : Vector2
  , position : Vector2
  , size : Int
  }


initRock : Rock
initRock =
  { velocity = { x = 5, y = 5 }
  , position = randomPosition
  , size = 3
  }

-- UPDATE

updateRock : Float -> Rock -> Rock
updateRock dt rock =
  { rock
  | position = updatePosition True dt rock.velocity rock.position
  }

-- VIEW

drawRock : Rock -> Form
drawRock rock =
  circle 20
    |> filled blue
    |> move (rock.position.x, rock.position.y)

module Rock where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Config exposing (..)
import Physics exposing (..)
import Random exposing (Seed)

-- MODEL

type alias Rock =
  { velocity : Vector2
  , position : Vector2
  , size : Int
  }


initRock : VectorRandomizer -> Rock
initRock randomizer =
  let
    randPosition = randomVector halfHeight halfWidth randomizer
  in
  { velocity = { x = 5, y = 5 }
  , position = randPosition.value
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

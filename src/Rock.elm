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
  , radius : Float
  }


initRock : Int -> Physics.Vector2 -> Physics.Vector2 -> Rock
initRock size velocity position =
  { velocity = velocity
  , position = position
  , size = size
  , radius = 25
  }


-- UPDATE

updateRock : Float -> Rock -> List Rock
updateRock dt rock =
  let
    newRock = splitRock False rock
  in
  List.map (updateRock' dt) newRock


updateRock' : Float -> Rock -> Rock
updateRock' dt rock =
  { rock
  | position = Physics.updatePosition
    True dt rock.velocity rock.position
  }


splitRock : Bool -> Rock -> List Rock
splitRock damage rock =
  if damage then
    [ initRock (rock.size - 1) rock.velocity rock.position
    , initRock (rock.size - 1) rock.velocity rock.position
    ]
  else
    [ rock ]


-- VIEW

drawRock : Rock -> Form
drawRock rock =
  circle rock.radius
    |> filled blue
    |> move (rock.position.x, rock.position.y)

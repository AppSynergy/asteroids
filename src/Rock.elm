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
  , color : Color
  }


initRock : Int -> Physics.Vector2 -> Physics.Vector2 -> Rock
initRock size velocity position =
  { velocity = velocity
  , position = position
  , size = size
  , radius = toFloat (8 * size)
  , color = black
  }


-- UPDATE

updateRock : Float -> Bool -> Rock -> List Rock
updateRock dt damage rock =
  let
    newRock = splitRock damage rock
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
  let
    body = circle rock.radius
      |> filled rock.color
    spot1 = circle (rock.radius / 5)
      |> filled darkRed
      |> move (8 , 11)
    spot2 = circle (rock.radius / 5)
      |> filled darkRed
      |> move (14 , -3)
    spot3 = circle (rock.radius / 5)
      |> filled darkRed
      |> move (-7 , -7)
  in
  group [body, spot1, spot2, spot3]
    |> move (rock.position.x, rock.position.y)

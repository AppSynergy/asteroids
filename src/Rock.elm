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
  , facing : Float
  , spinRate : Float
  }


initRock : Int -> Float -> Physics.Vector2 -> Physics.Vector2 -> Rock
initRock size spin velocity position =
  { velocity = velocity
  , position = position
  , size = size
  , radius = toFloat (8 * size)
  , color = black
  , facing = 0
  , spinRate = spin
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
  let
    spinningRock = updateFacing dt rock
  in
  { spinningRock
  | position = Physics.updatePosition
    True dt rock.velocity rock.position
  }


updateFacing : Float -> Rock -> Rock
updateFacing dt rock =
  { rock
  | facing = rock.facing + (dt / 10)
  }


splitRock : Bool -> Rock -> List Rock
splitRock damage rock =
  if damage && rock.size == 1 then [] else
  if damage then
    [ initRock (rock.size - 1) rock.spinRate rock.velocity rock.position
    , initRock (rock.size - 1) rock.spinRate rock.velocity rock.position
    ]
  else
    [ rock ]


changeColor : Rock -> Rock
changeColor rock =
  { rock | color = blue }


-- VIEW

drawRock : Rock -> Form
drawRock rock =
  let
    body = circle rock.radius
      |> filled rock.color
    spot1 = circle (rock.radius / 5)
      |> filled darkRed
      |> move (rock.radius / 3 , rock.radius / 2)
    spot2 = circle (rock.radius / 4)
      |> filled darkRed
      |> move (rock.radius / -2 , rock.radius / 3.5)
    spot3 = circle (rock.radius / 7)
      |> filled darkRed
      |> move (rock.radius / -3 , rock.radius / -1.6)
  in
  group [body, spot1, spot2, spot3]
    |> rotate (degrees rock.facing)
    |> move (rock.position.x, rock.position.y)

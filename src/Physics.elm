module Physics where

import Config exposing (halfWidth, halfHeight)
import Random exposing (..)

-- MODEL

type alias Vector2 =
  { x : Float
  , y : Float
  }


type alias VectorRandomizer =
  { seed : Seed
  , value : Vector2
  }


initVectorRandomizer : VectorRandomizer
initVectorRandomizer =
  { seed = Random.initialSeed 55
  , value = { x = 0, y = 0 }
  }


randomGenerator : Float -> Generator Float
randomGenerator bound =
  Random.float (negate bound) bound


randomVector : Float -> Float -> VectorRandomizer -> VectorRandomizer
randomVector ybound xbound model =
  let
    genY = randomGenerator ybound
    genX = randomGenerator xbound
    rY = Random.generate genY model.seed
    rX = Random.generate genX (snd rY)
  in
    { model
    | seed = (snd rX)
    , value = { x = (fst rX), y = (fst rY) }
    }

-- UPDATE

updatePosition : Bool -> Float -> Vector2 -> Vector2 -> Vector2
updatePosition isWrappable dt velocity position =
  let
    scale = 0.01
    newPositionY = position.y + velocity.y * scale * dt
    newPositionX = position.x + velocity.x * scale * dt
    newPositionY' = if isWrappable then
      wrapGeometry newPositionY halfHeight
    else
      newPositionY
    newPositionX' = if isWrappable then
      wrapGeometry newPositionX halfWidth
    else
      newPositionX
  in
  { position
  | y = newPositionY' |> floor >> toFloat
  , x = newPositionX' |> floor >> toFloat
  }


wrapGeometry : Float -> Float -> Float
wrapGeometry position dimension =
  if position > dimension then
    -dimension
  else if position < -dimension then
    dimension
  else position

module Generic where

(gameWidth, gameHeight) = (620, 480)
(halfWidth, halfHeight) = (310, 240)

-- http://package.elm-lang.org/packages/elm-lang/core/3.0.0/Keyboard#arrows
type alias KeyInput =
  { x : Int
  , y : Int
  }


type alias Vector2 =
  { x : Float
  , y : Float
  }

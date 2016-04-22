module UI where

import Keyboard


-- MODEL

(gameWidth, gameHeight) = (620, 480)
(halfWidth, halfHeight) = (310, 240)


type alias KeyInput =
  { x : Int
  , y : Int
  }


ui =
  { steering = Signal.merge Keyboard.wasd Keyboard.arrows
  , firing = Keyboard.space
  }

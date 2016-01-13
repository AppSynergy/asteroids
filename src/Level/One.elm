module Level.One where

import Rock exposing (Rock)
import Randomizer exposing (Randomizer)

-- MODEL

type alias Level =
  { number : Int
  , rand : Randomizer
  , rocks : List Rock
  }

init : Level
init =
  let
    r = Randomizer.init 987654
  in
  { number = 1
  , rand = r
  , rocks = someRocks r
  }

someRocks rand =
  let
    rockPositions =
      ( { x = 50, y = 45 }
      , { x = -134, y = 208 }
      )
    rockVelocities =
      ( { x = 4, y = -9 }
      , { x = -13, y = 8 }
      )
  in
  [ Rock.init 3 25 (fst rockVelocities) (fst rockPositions)
  , Rock.init 3 9 (snd rockVelocities) (snd rockPositions)
  ]

module Randomizer where

import Random
import Physics


-- MODEL

type alias Randomizer =
  { seed : Random.Seed
  , value : Float
  }


init : Int -> Randomizer
init int =
  { seed = Random.initialSeed int
  , value = 42
  }


generator : Float -> Random.Generator Float
generator bound =
  Random.float (negate bound) bound


-- UPDATE

update : Float -> Randomizer -> Randomizer
update bound randomizer =
  let
    (result,seed) = Random.generate (generator bound) randomizer.seed
  in
  { randomizer
  | seed = seed
  , value = result
  }

module Randomizer where

import Random

{--
r0 = Randomizer.init 567
r1 = Randomizer.update gameWidth r0
d1 = Debug.watch "rand2" r1.value
r2 = Randomizer.update gameHeight r1
d2 = Debug.watch "rand4" r2.value
--}

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

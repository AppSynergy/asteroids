module Level.One where

import Rock exposing (Rock)
import Randomizer exposing (Randomizer)
import UI exposing (gameWidth, gameHeight)

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
    vmax = 10
    r1 = Randomizer.update gameWidth rand
    r2 = Randomizer.update gameHeight r1
    r3 = Randomizer.update gameWidth r2
    r4 = Randomizer.update gameHeight r3
    r5 = Randomizer.update vmax r4
    r6 = Randomizer.update vmax r5
    r7 = Randomizer.update vmax r6
    r8 = Randomizer.update vmax r7
  in
  [ Rock.init 3 25 { x = r5.value, y = r6.value } { x = r1.value, y = r2.value }
  , Rock.init 3 9 { x = r7.value, y = r8.value } { x = r3.value, y = r4.value }
  ]

module Level.One where

import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Randomizer exposing (Randomizer)
import UI exposing (gameWidth, gameHeight)


-- MODEL

type alias Level =
  { number : Int
  , rand : Randomizer
  , rocks : List Rock
  , saucers : List Saucer
  }


init : Level
init =
  let
    r = Randomizer.init 987654
  in
  { number = 1
  , rand = r
  , rocks = rocks r
  , saucers = saucers r
  }


saucers : Randomizer -> List Saucer
saucers rand =
  let
    r1 = Randomizer.update gameWidth rand
    r2 = Randomizer.update gameHeight r1
  in
  [ Saucer.init 1 { x = r1.value, y = r2.value }
  ]


rocks : Randomizer -> List Rock
rocks rand =
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
    r9 = Randomizer.update gameWidth r8
    r10 = Randomizer.update gameHeight r9
    r11 = Randomizer.update vmax r10
    r12 = Randomizer.update vmax r11
  in
  [ Rock.init 3 25 { x = r5.value, y = r6.value } { x = r1.value, y = r2.value }
  , Rock.init 3 9 { x = r7.value, y = r8.value } { x = r3.value, y = r4.value }
  , Rock.init 3 14 { x = r11.value, y = r12.value } { x = r9.value, y = r10.value }
  ]

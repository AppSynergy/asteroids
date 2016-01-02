module Asteroids where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Keyboard

(gameWidth, gameHeight) = (620,480)

-- MODEL

type Input
  = TurnLeft
  | TurnRight
  | Thrust
  | FireWeapon

type alias Ship =
  { firing : Bool }

initShip : Ship
initShip =
  { firing = False }

-- UPDATE

-- VIEW

-- SIGNALS

main : Element
main =
    show 42

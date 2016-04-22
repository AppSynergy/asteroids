module Asteroids where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Time

import Loop
import Model
import UI exposing (ui)
import Sprite.Default as Sprite exposing (Sprite)

import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Overlay.Message as Message exposing (Message)


main : Signal Element.Element
main =
  Signal.map view gameState


gameState : Signal Model.Game
gameState =
  Signal.foldp Loop.run Model.init input


input : Signal (Float, UI.KeyInput, Bool)
input =
  let
    delta = Time.fps 30
    tuples = Signal.map3 (,,) delta ui.steering ui.firing
  in
  Signal.sampleOn delta tuples


view : Model.Game -> Element.Element
view game =
  let
    background = Draw.rect UI.gameWidth UI.gameHeight
      |> Draw.filled game.backgroundColor
    allForms = List.concat
      [ [ background ]
      , Sprite.draw game
      , Scoreboard.draw game.scoreboard
      , Message.draw game.loseMessage
      , Message.draw game.startMessage
      ]
  in
  Draw.collage UI.gameWidth UI.gameHeight allForms
    |> Element.container UI.gameWidth UI.gameHeight Element.middle

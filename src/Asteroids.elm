module Asteroids where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Time

import UI exposing (gameWidth, gameHeight, KeyInput, ui)
import Ship exposing (Ship)
import Bullet exposing (Bullet)
import Rock exposing (Rock)
import Explosion.Explosion as Explosion exposing (Explosion)
import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Physics
import Level.One


-- MODEL

type alias Game =
  { ship : Ship
  , bullets : List Bullet
  , rocks : List Rock
  , explosions : List Explosion
  , scoreboard : Scoreboard
  , backgroundColor : Color.Color
  }


initGame : Game
initGame =
  let
    level = Level.One.init
  in
  { ship = Ship.init
  , bullets = []
  , rocks = level.rocks
  , explosions = []
  , scoreboard = Scoreboard.init
  , backgroundColor = Color.black
  }


-- UPDATE

update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    bullets = Bullet.fire game.ship game.bullets

    bulletCollideRock = List.map
      (detectCollisions game.rocks) bullets

    rockCollideShip = List.map
      (Physics.collides game.ship) game.rocks

    shipHit = if game.ship.invulnerable then False
      else Physics.hitAny rockCollideShip

    d1 = Debug.watch "shipHit" shipHit

    newBullets = bullets
      |> List.filterMap (Bullet.update dt)
      |> Bullet.removeDead (Bullet.onTarget bulletCollideRock)

    newRocks = game.rocks
      |> List.map2 (Rock.update dt)
        (Rock.damaged (List.length game.rocks) bulletCollideRock)
      |> List.concat

    newExplosions = game.explosions
      |> List.filterMap (Explosion.update dt)
      |> Explosion.create
        (Physics.getCollidePositions (List.concat bulletCollideRock))

    shipExplosions = if shipHit then
      newExplosions
        |> Explosion.create
          (Physics.getCollidePositions rockCollideShip)
    else
      newExplosions

    newScoreboard =
      Scoreboard.update bulletCollideRock shipHit game.scoreboard

  in
  { game
  | ship = Ship.update (dt, keyInput, fireInput) game.ship
  , bullets = newBullets
  , rocks = newRocks
  , explosions = shipExplosions
  , scoreboard = newScoreboard
  }


detectCollisions : List (Physics.Collidable Rock) -> Bullet
  -> List (Physics.CollisionResult Rock)
detectCollisions targets bullet =
  List.map (Physics.collides bullet) targets


-- VIEW

view : Game -> Element.Element
view game =
  let
    background = Draw.rect gameWidth gameHeight
      |> Draw.filled game.backgroundColor
    allForms = List.concat
      [ [ background ]
      , List.map Bullet.draw game.bullets
      , List.map Rock.draw game.rocks
      , List.map Explosion.draw game.explosions
      , Scoreboard.draw game.scoreboard
      , [ Ship.draw game.ship ]
      ]
  in
  Element.container gameWidth gameHeight Element.middle <|
    Draw.collage gameWidth gameHeight allForms


-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update initGame inputSignal


inputSignal : Signal (Float, KeyInput, Bool)
inputSignal =
  let
    delta = Time.fps 30
    tuples = Signal.map3 (,,) delta ui.steering ui.firing
  in
  Signal.sampleOn delta tuples


main : Signal Element.Element
main =
  Signal.map view gameState

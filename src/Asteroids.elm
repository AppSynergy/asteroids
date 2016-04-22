module Asteroids where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Time

import Model exposing (Game)
import Sprite.Default as Sprite exposing (Sprite)
import UI exposing (gameWidth, gameHeight, KeyInput, ui)

import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Entity.Destroyable as Damage

import Particle.ExplosionManager as ExpMan
import Particle.Bullet as Bullet exposing (Bullet)
import Particle.Explosion as Explosion exposing (Explosion)

import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Overlay.Message as Message exposing (Message)

import Physics


-- UPDATE

{-| This is the core update function, equivilent to the game loop.
-}
update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    -- Determine if  bullets have collided with rocks.
    bulletCollideRock : Physics.CollisionMatrix Rock
    bulletCollideRock =
      let
        col targets bullet =
          List.map (Physics.collides bullet) targets
      in
      List.map (col game.rocks) game.bullets

    -- Determine if bullets have collided with saucers.
    bulletCollideSaucer =
      let
        col targets bullet =
          List.map (Physics.collides bullet) targets
      in
      List.map (col game.saucers) game.bullets

    -- Determine if any rocks have collided with the ship.
    rockCollideShip : List (Physics.CollisionResult Rock)
    rockCollideShip =
      List.map (Physics.collides game.ship) game.rocks


    bullets' : List Bullet
    bullets' = game.bullets
      |> List.filterMap (Bullet.update dt)
      |> Bullet.removeDead (Bullet.onTarget bulletCollideRock)
      |> Bullet.removeDead (Bullet.onTarget bulletCollideSaucer)
      |> Bullet.fire game.ship "default"

    -- Determine if the ship is going to lose a life.
    shipHit : Bool
    shipHit = if game.ship.invulnerable then False
      else Physics.hitAny rockCollideShip

    saucerBullets : List Bullet
    saucerBullets = game.saucers
      |> List.map (\x -> Bullet.fire x "saucer" game.saucerBullets )
      |> List.concat

    newSaucerBullets : List Bullet
    newSaucerBullets = saucerBullets
      |> List.filterMap (Bullet.update dt)


    explosions' : List Explosion
    explosions' =
      [ Physics.getCollidePositions (List.concat bulletCollideRock)
      , Physics.getCollidePositions (List.concat bulletCollideSaucer)
      , Physics.getCollidePositions rockCollideShip
      ]
        --|> List.map Physics.getCollidePositions
        |> List.concat
        |> ExpMan.update dt game.explosions


    rocks' : List Rock
    rocks' =
      Damage.damaged (List.length game.rocks) bulletCollideRock
        |> List.map2 (Rock.update dt) game.rocks
        |> List.concat


    saucers' : List Saucer
    saucers' =
      Damage.damaged (List.length game.saucers) bulletCollideSaucer
        |> List.map2 (Saucer.update dt game.ship) game.saucers
        |> List.concat


    gameship : Ship
    gameship = game.ship

    newShip : Ship
    newShip =
      -- Enable the ship if the game is started.
      if not game.playing && fireInput then
        { gameship | dead = False }
      -- Disable the ship if you run out of lives.
      else if game.scoreboard.lives == 0 then
        { gameship | dead = True }
      else
      -- Otherwise, update the ship, losing a life if necessary.
        gameship
          |> Ship.update  (dt, keyInput, fireInput)
          |> Ship.loseLife shipHit
  in
  -- Update the game model using all the above results.
  { game
  | ship = newShip
  , bullets = bullets'
  , saucerBullets = newSaucerBullets
  , rocks = rocks'
  , saucers = saucers'
  , explosions = explosions'
  -- Update messages & scoreboards.
  , scoreboard = game.scoreboard
    |> Scoreboard.update bulletCollideRock shipHit
  , loseMessage = game.loseMessage
    |> Message.update (game.scoreboard.lives == 0)
  , startMessage = game.startMessage
    |> Message.update (not game.playing)
  , playing = game.playing || fireInput
  }



-- VIEW

view : Game -> Element.Element
view game =
  let
    background = Draw.rect gameWidth gameHeight
      |> Draw.filled game.backgroundColor
    allForms = List.concat
      [ [ background ]
      , Sprite.draw game
      , Scoreboard.draw game.scoreboard
      , Message.draw game.loseMessage
      , Message.draw game.startMessage
      ]
  in
  Element.container gameWidth gameHeight Element.middle <|
    Draw.collage gameWidth gameHeight allForms


-- SIGNALS

gameState : Signal Game
gameState =
  Signal.foldp update Model.init inputSignal


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

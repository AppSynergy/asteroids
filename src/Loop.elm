module Loop where

import UI
import Model
import Physics

import Entity.Destroyable as Damage
import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)

import Particle.ExplosionManager as ExpMan
import Particle.Bullet as Bullet exposing (Bullet)
import Particle.Explosion as Explosion exposing (Explosion)

import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Overlay.Message as Message exposing (Message)


run : (Float, UI.KeyInput, Bool) -> Model.Game -> Model.Game
run (dt, keyInput, fireInput) game =
  let
    bulletCollideRock : Physics.CollisionMatrix Rock
    bulletCollideRock =
      let
        col targets bullet =
          List.map (Physics.collides bullet) targets
      in
      List.map (col game.rocks) game.bullets


    bulletCollideSaucer =
      let
        col targets bullet =
          List.map (Physics.collides bullet) targets
      in
      List.map (col game.saucers) game.bullets


    rockCollideShip : List (Physics.CollisionResult Rock)
    rockCollideShip =
      List.map (Physics.collides game.ship) game.rocks
  in
  { game
  | ship = ship' game (dt, keyInput, fireInput) rockCollideShip
  , bullets = bullets' game dt (bulletCollideRock, bulletCollideSaucer)
  , saucerBullets = saucerBullets' game dt
  , rocks = rocks' game dt bulletCollideRock
  , saucers = saucers' game dt bulletCollideSaucer
  , explosions = explosions' game dt (bulletCollideRock, bulletCollideSaucer, rockCollideShip)
  , scoreboard = scoreboard' game (bulletCollideRock, rockCollideShip)
  , loseMessage = loseMessage' game
  , startMessage = startMessage' game
  , playing = playing' game fireInput
  }


type alias Hits a = Physics.CollisionMatrix a

-- MODEL RECORD COMPONENT UPDATES

bullets' : Model.Game -> Float -> (Hits Rock, Hits a) -> List Bullet
bullets' game dt (rocks, saucers) =
  game.bullets
    |> List.filterMap (Bullet.update dt)
    |> Bullet.removeDead (Bullet.onTarget rocks)
    |> Bullet.removeDead (Bullet.onTarget saucers)
    |> Bullet.fire game.ship "default"


saucerBullets' : Model.Game -> Float -> List Bullet
saucerBullets' game dt =
  game.saucers
    |> List.map (\x -> Bullet.fire x "saucer" game.saucerBullets )
    |> List.concat
    |> List.filterMap (Bullet.update dt)


--explosions' : Model.Game -> Float -> List Explosion
explosions' game dt (rocks, saucers, ship) =
  [ Physics.getCollidePositions (List.concat rocks)
  , Physics.getCollidePositions (List.concat saucers)
  , Physics.getCollidePositions ship
  ]
    |> List.concat
    |> ExpMan.update dt game.explosions


rocks' : Model.Game -> Float -> Hits Rock -> List Rock
rocks' game dt bulletCollideRock =
  Damage.damaged (List.length game.rocks) bulletCollideRock
    |> List.map2 (Rock.update dt) game.rocks
    |> List.concat


saucers' : Model.Game -> Float -> Hits a -> List Saucer
saucers' game dt bulletCollideSaucer =
  Damage.damaged (List.length game.saucers) bulletCollideSaucer
    |> List.map2 (Saucer.update dt game.ship) game.saucers
    |> List.concat


ship' : Model.Game -> (Float, UI.KeyInput, Bool) -> List (Physics.CollisionResult b) -> Ship
ship' game (dt, keyInput, fireInput) rockCollideShip =
  let
    ship = game.ship
    shipHit =
      if game.ship.invulnerable then False
      else Physics.hitAny rockCollideShip
  in
  if not game.playing && fireInput then
    { ship | dead = False } -- Enable the ship, game started.
  else if game.scoreboard.lives == 0 then
    { ship | dead = True } -- Disable the ship, out of lives.
  else
    ship
      |> Ship.update (dt, keyInput, fireInput)
      |> Ship.loseLife shipHit


scoreboard' : Model.Game -> (Hits Rock, List (Physics.CollisionResult b)) -> Scoreboard
scoreboard' game (bulletCollideRock, rockCollideShip) =
  let
    ship = game.ship
    shipHit =
      if game.ship.invulnerable then False
      else Physics.hitAny rockCollideShip
  in
  game.scoreboard
    |> Scoreboard.update bulletCollideRock shipHit


loseMessage' game =
  game.loseMessage
    |> Message.update (game.scoreboard.lives == 0)


startMessage' game =
  game.startMessage
    |> Message.update (not game.playing)


playing' game fireInput =
  game.playing || fireInput

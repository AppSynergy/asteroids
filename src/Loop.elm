module Loop where

import UI
import Model exposing (Game)
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


-- HELPER TYPES FOR SIGNATURES IN COMPONENTS

type alias Hits a = List (Physics.CollisionResult a)
type alias Hits' a = Physics.CollisionMatrix a
type alias Obj a = Physics.Collidable a -- ??

-- RUN MAIN LOOP

run : (Float, UI.KeyInput, Bool) -> Game -> Game
run (dt, keyInput, fireInput) game =
  let

    bulletCollides : List (Physics.Collidable a) -> Obj b -> Hits a
    bulletCollides targets bullet =
      List.map (Physics.collides bullet) targets

    bulletCollideRock : Hits' Rock
    bulletCollideRock =
      List.map (bulletCollides game.rocks) game.bullets

    --bulletCollideSaucer : List (List (Physics.CollisionResult b))
    bulletCollideSaucer =
      List.map (bulletCollides game.saucers) game.bullets

    rockCollideShip : Hits Rock
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
  , message = message' game
  , playing = playing' game fireInput
  }


-- MODEL RECORD COMPONENT UPDATES

ship' : Game -> (Float, UI.KeyInput, Bool) -> Hits Rock -> Ship
ship' game (dt, keyInput, fireInput) hits =
  let
    ship = game.ship
    shipHit = if ship.invulnerable then False
      else Physics.hitAny hits
  in
  if not game.playing && fireInput then
    { ship | dead = False } -- Enable the ship, game started.
  else if game.scoreboard.lives == 0 then
    { ship | dead = True } -- Disable the ship, out of lives.
  else
    ship
      |> Ship.update (dt, keyInput, fireInput)
      |> Ship.loseLife shipHit


bullets' : Game -> Float -> (Hits' Rock, Hits' a) -> List Bullet
bullets' game dt (rocks, saucers) =
  game.bullets
    |> List.filterMap (Bullet.update dt)
    |> Bullet.removeDead (Bullet.onTarget rocks)
    |> Bullet.removeDead (Bullet.onTarget saucers)
    |> Bullet.fire game.ship "default"


saucerBullets' : Game -> Float -> List Bullet
saucerBullets' game dt =
  game.saucers
    |> List.map (\x -> Bullet.fire x "saucer" game.saucerBullets )
    |> List.concat
    |> List.filterMap (Bullet.update dt)


explosions' : Game -> Float -> (Hits' Rock, Hits' b, Hits Rock) -> List Explosion
explosions' game dt (rocks, saucers, ship) =
  [ Physics.getCollidePositions (List.concat rocks)
  , Physics.getCollidePositions (List.concat saucers)
  , Physics.getCollidePositions ship
  ]
    |> List.concat
    |> ExpMan.update dt game.explosions


rocks' : Game -> Float -> Hits' Rock -> List Rock
rocks' game dt rocks =
  Damage.damaged (List.length game.rocks) rocks
    |> List.map2 (Rock.update dt) game.rocks
    |> List.concat


saucers' : Game -> Float -> Hits' a -> List Saucer
saucers' game dt saucers =
  Damage.damaged (List.length game.saucers) saucers
    |> List.map2 (Saucer.update dt game.ship) game.saucers
    |> List.concat


scoreboard' : Game -> (Hits' Rock, Hits b) -> Scoreboard
scoreboard' game (rocks, hits) =
  let
    ship = game.ship
    shipHit = if game.ship.invulnerable then False
      else Physics.hitAny hits
  in
  game.scoreboard
    |> Scoreboard.update rocks shipHit


message' : Game -> Message
message' game =
  let update = Message.update game.message in
  if game.scoreboard.lives == 0 then
    update True "GAME OVER"
  else
    update (not game.playing) "PRESS SPACE TO START"


playing' : Game -> Bool -> Bool
playing' game fireInput =
  game.playing || fireInput

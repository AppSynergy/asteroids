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

type alias HitShip = List (Physics.CollisionResult Ship)
type alias HitRocks = Physics.CollisionMatrix Rock
type alias HitSaucers = Physics.CollisionMatrix Saucer


-- RUN MAIN LOOP

run : (Float, UI.KeyInput, Bool) -> Game -> Game
run (dt, keyInput, fireInput) game =
  let

    hitRocks : HitRocks
    hitRocks =
      List.map (Physics.listCollides game.rocks) game.bullets

    hitSaucers : HitSaucers
    hitSaucers =
      List.map (Physics.listCollides game.saucers) game.bullets

    hitShip : HitShip
    hitShip =
      List.map (\rock -> Physics.collides rock game.ship) game.rocks

  in

  { game
  | ship = ship' game (dt, keyInput, fireInput) hitShip
  , bullets = bullets' game dt (hitRocks, hitSaucers)
  , saucerBullets = saucerBullets' game dt
  , rocks = rocks' game dt hitRocks
  , saucers = saucers' game dt hitSaucers
  , explosions = explosions' game dt (hitRocks, hitSaucers, hitShip)
  , scoreboard = scoreboard' game (hitRocks, hitShip)
  , message = message' game
  , playing = playing' game fireInput
  }


-- MODEL RECORD COMPONENT UPDATES

ship' : Game -> (Float, UI.KeyInput, Bool) -> HitShip -> Ship
ship' game (dt, keyInput, fireInput) hits =
  let
    ship = game.ship
    shipHit = if ship.invulnerable then False
      else Physics.hitAny hits
  in
  if not game.playing && fireInput then -- game started
    { ship | dead = False }
  else if game.scoreboard.lives == 0 then -- out of lives
    { ship | dead = True }
  else
    ship
      |> Ship.update (dt, keyInput, fireInput)
      |> Ship.loseLife shipHit


bullets' : Game -> Float -> (HitRocks, HitSaucers) -> List Bullet
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


explosions' : Game -> Float -> (HitRocks, HitSaucers, HitShip) -> List Explosion
explosions' game dt (rocks, saucers, ship) =
  [ Physics.getCollidePositions (List.concat rocks)
  , Physics.getCollidePositions (List.concat saucers)
  , Physics.getCollidePositions ship
  ]
    |> List.concat
    |> ExpMan.update dt game.explosions


rocks' : Game -> Float -> HitRocks -> List Rock
rocks' game dt rocks =
  Damage.damaged (List.length game.rocks) rocks
    |> List.map2 (Rock.update dt) game.rocks
    |> List.concat


saucers' : Game -> Float -> HitSaucers -> List Saucer
saucers' game dt saucers =
  Damage.damaged (List.length game.saucers) saucers
    |> List.map2 (Saucer.update dt game.ship) game.saucers
    |> List.concat


scoreboard' : Game -> (HitRocks, HitShip) -> Scoreboard
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

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

type alias HitRocks = Physics.CollisionMatrix Rock
type alias HitSaucers = Physics.CollisionMatrix Saucer
type alias HitByRocks = Physics.CollisionMatrix Ship
type alias HitBySaucers = Physics.CollisionMatrix Ship


-- RUN MAIN LOOP

run : (Float, UI.KeyInput, Bool) -> Game -> Game
run (dt, keyInput, fireInput) game =
  let

    hitRocks : HitRocks
    hitRocks = Physics.collideLists game.bullets game.rocks

    hitSaucers : HitSaucers
    hitSaucers = Physics.collideLists game.bullets game.saucers

    hitByRocks : HitByRocks
    hitByRocks =  Physics.collideLists game.rocks [game.ship]

    hitBySaucers : HitBySaucers
    hitBySaucers = Physics.collideLists game.saucerBullets [game.ship]

    shipHits : Bool
    shipHits = Physics.hitMatrixList [hitByRocks,hitBySaucers]

  in

  { game
  | ship = ship' game (dt, keyInput, fireInput) shipHits
  , bullets = bullets' game dt (hitRocks, hitSaucers)
  , saucerBullets = saucerBullets' game dt hitBySaucers
  , rocks = rocks' game dt hitRocks
  , saucers = saucers' game dt hitSaucers
  , explosions = explosions' game dt (hitRocks, hitSaucers, shipHits)
  , scoreboard = scoreboard' game (hitRocks, shipHits)
  , message = message' game
  , playing = playing' game fireInput
  }


-- MODEL RECORD COMPONENT UPDATES

ship' : Game -> (Float, UI.KeyInput, Bool) -> Bool -> Ship
ship' game (dt, keyInput, fireInput) shipHits =
  --let ship = game.ship in
  --if not game.playing && fireInput then -- game started
  --  { ship | dead = False }
  --else if game.scoreboard.lives == 0 then -- out of lives
  --  { ship | dead = True }
  --else
  Ship.update (dt, keyInput, fireInput) game.ship
    |> Ship.loseLife shipHits


bullets' : Game -> Float -> (HitRocks, HitSaucers) -> List Bullet
bullets' game dt (rocks, saucers) =
  game.bullets
    |> List.filterMap (Bullet.update dt)
    |> Bullet.removeDead (Bullet.onTarget rocks)
    |> Bullet.removeDead (Bullet.onTarget saucers)
    |> Bullet.fire game.ship "default"


saucerBullets' : Game -> Float -> HitBySaucers -> List Bullet
saucerBullets' game dt bySaucers =
  game.saucers
    |>
      ( game.saucerBullets
        |> List.filterMap (Bullet.update dt)
        |> List.foldr (\s -> Bullet.fire s "saucer")
      )
    |> Bullet.removeDead [Physics.hitAny (List.concat bySaucers)]



explosions' : Game -> Float -> (HitRocks, HitSaucers, Bool) -> List Explosion
explosions' game dt (rocks, saucers, shipHits) =
  [ Physics.getCollidePositions (List.concat rocks)
  , Physics.getCollidePositions (List.concat saucers)
  ]
    |> List.concat
    |> List.append (if shipHits then [game.ship.position] else [])
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


scoreboard' : Game -> (HitRocks, Bool) -> Scoreboard
scoreboard' game (rocks, shipHits) =
  let
    ship = game.ship
    shipHit = if game.ship.invulnerable then False else shipHits
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

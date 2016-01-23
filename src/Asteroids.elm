module Asteroids where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Time

import Skin.Default as Skin
import UI exposing (gameWidth, gameHeight, KeyInput, ui)
import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Particle.Bullet as Bullet exposing (Bullet)
import Particle.Explosion as Explosion exposing (Explosion)
import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Overlay.Message as Message exposing (Message)
import Physics
import Level.One


-- MODEL

type alias Game =
  { ship : Ship
  , bullets : List Bullet
  , rocks : List Rock
  , saucers : List Saucer
  , explosions : List Explosion
  , scoreboard : Scoreboard
  , backgroundColor : Color.Color
  , loseMessage : Message
  , startMessage : Message
  , playing : Bool
  }


initGame : Game
initGame =
  let
    level = Level.One.init
  in
  { ship = Ship.init
  , bullets = []
  , rocks = level.rocks
  , saucers = level.saucers
  , explosions = []
  , scoreboard = Scoreboard.init
  , backgroundColor = Color.black
  , loseMessage = Message.init False "GAME OVER"
  , startMessage = Message.init True "PRESS SPACE TO START"
  , playing = False
  }


-- UPDATE

{-| This is the core update function, equivilent to the game loop.
-}
update : (Float, KeyInput, Bool) -> Game -> Game
update (dt, keyInput, fireInput) game =
  let
    -- Determine if a new bullet has been fired.
    bullets = Bullet.fire game.ship game.bullets

    -- Determine if any bullets have collided with any rocks.
    bulletCollideRock = List.map
      (detectCollisions game.rocks) bullets

    -- Determine if any rocks have collided with the ship.
    rockCollideShip = List.map
      (Physics.collides game.ship) game.rocks

    -- Determine if the ship is going to lose a life.
    shipHit = if game.ship.invulnerable then False
      else Physics.hitAny rockCollideShip

    -- Update each bullet, and remove bullets which have expired.
    newBullets = bullets
      |> List.filterMap (Bullet.update dt)
      |> Bullet.removeDead (Bullet.onTarget bulletCollideRock)

    -- Update each rock, splitting them up or removing them if necessary.
    newRocks = game.rocks
      |> List.map2 (Rock.update dt)
        (Rock.damaged (List.length game.rocks) bulletCollideRock)
      |> List.concat

    -- Update each explosion, adding new ones where rocks are hit.
    rockExplosions = game.explosions
      |> List.filterMap (Explosion.update dt)
      |> Explosion.create
        (Physics.getCollidePositions (List.concat bulletCollideRock))

    -- Add an additional explosion if the ship has been hit.
    shipExplosions =
      if shipHit then
        rockExplosions |> Explosion.create
          (Physics.getCollidePositions rockCollideShip)
      else
        rockExplosions

    -- Update each saucer.
    newSaucers = List.map (Saucer.update dt) game.saucers

    -- Update the ship.
    gameship = game.ship
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
  , bullets = newBullets
  , rocks = newRocks
  , saucers = newSaucers
  , explosions = shipExplosions
  -- Update messages & scoreboards.
  , scoreboard = game.scoreboard
    |> Scoreboard.update bulletCollideRock shipHit
  , loseMessage = game.loseMessage
    |> Message.update (game.scoreboard.lives == 0)
  , startMessage = game.startMessage
    |> Message.update (not game.playing)
  , playing = game.playing || fireInput
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
      , List.map Skin.drawBullet game.bullets
      , List.map Skin.drawRock game.rocks
      , List.map Saucer.draw game.saucers
      , List.map Explosion.draw game.explosions
      , Scoreboard.draw game.scoreboard
      , Message.draw game.loseMessage
      , Message.draw game.startMessage
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

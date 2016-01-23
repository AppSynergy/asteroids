module Model where

import Color

import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Particle.Bullet as Bullet exposing (Bullet)
import Particle.Explosion as Explosion exposing (Explosion)
import Overlay.Scoreboard as Scoreboard exposing (Scoreboard)
import Overlay.Message as Message exposing (Message)
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


init : Game
init =
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

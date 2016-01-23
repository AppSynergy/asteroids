module Particle.Explosion where

import Color

import Physics
import Particle.Fragment as Fragment exposing (Fragment)

-- MODEL

type alias Explosion =
  { position : Physics.Vector2
  , fragments : List Fragment
  , color: Color.Color
  , lifetime : Int
  }


init : Physics.Vector2 -> Explosion
init position =
  let
    angles = List.scanl (+) 0 (List.repeat 9 36)
    velocities = List.map (Physics.toVector 16) angles
  in
  { position = position
  , fragments = List.map (Fragment.init position) velocities
  , color = Color.yellow
  , lifetime = 12
  }


-- UPDATE

create : List Physics.Vector2 -> List Explosion -> List Explosion
create positions explosions =
  List.map init positions
    |> List.append explosions


update : Float -> Explosion -> Maybe Explosion
update dt explosion =
  { explosion
  | fragments = List.map (Fragment.update dt) explosion.fragments
  }
    |> Physics.expiration

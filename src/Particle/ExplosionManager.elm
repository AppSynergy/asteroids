module Particle.ExplosionManager where

import Particle.Explosion as Explosion exposing (Explosion)
import Physics


update : Float -> List Explosion -> List Physics.Vector2 -> List Explosion
update dt explosions collisions =
  explosions
    |> Explosion.create collisions
    |> List.filterMap (Explosion.update dt)


create :  List (Physics.CollisionResult a) -> List Explosion -> List Explosion
create collision explosions =
  explosions |> Explosion.create (Physics.getCollidePositions collision)

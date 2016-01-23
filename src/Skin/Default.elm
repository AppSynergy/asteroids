module Skin.Default where

import Color
import Graphics.Collage as Draw

import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Particle.Explosion as Explosion exposing (Explosion)
import Particle.Fragment as Fragment exposing (Fragment)
import Particle.Bullet as Bullet exposing (Bullet)

type Skin
  = ShipSkin Ship
  | RockSkin Rock
  | BulletSkin Bullet
  | SaucerSkin Saucer
  | ExplosionSkin Explosion


draw : Skin -> Draw.Form
draw object =
  case object of

    ShipSkin ship ->
      let
        itime = ship.invulnerableCounter
        flashColor = if itime > 0 then
          if itime % 5 == 0 then Color.white else ship.color1
        else
          ship.color1
        triangle = Draw.ngon 3 32
          |> Draw.filled flashColor
        engines = Draw.rect 4 32
          |> Draw.filled ship.color2
          |> Draw.move (-18 , 0)
        dline = Draw.defaultLine
        bound = Draw.circle ship.radius
          |> Draw.outlined { dline | color = Color.red }
      in
      if ship.dead then
        Draw.circle 0
          |> Draw.filled Color.black
      else
        Draw.group [ triangle , engines , bound ]
          |> Draw.rotate ( degrees (ship.facing + 90 ))
          |> Draw.move (ship.position.x, ship.position.y)

    RockSkin rock ->
      let
        body = Draw.circle rock.radius
          |> Draw.filled rock.color1
        spot1 = Draw.circle (rock.radius / 5)
          |> Draw.filled rock.color2
          |> Draw.move (rock.radius / 3 , rock.radius / 2)
        spot2 = Draw.circle (rock.radius / 4)
          |> Draw.filled rock.color2
          |> Draw.move (rock.radius / -2 , rock.radius / 3.5)
        spot3 = Draw.circle (rock.radius / 7)
          |> Draw.filled rock.color2
          |> Draw.move (rock.radius / -3 , rock.radius / -1.6)
      in
      Draw.group [body, spot1, spot2, spot3]
        |> Draw.rotate (degrees rock.facing)
        |> Draw.move (rock.position.x, rock.position.y)

    BulletSkin bullet ->
      Draw.circle bullet.radius
        |> Draw.filled bullet.color
        |> Draw.move (bullet.position.x, bullet.position.y)

    SaucerSkin saucer ->
      Draw.circle saucer.radius
        |> Draw.filled Color.darkRed
        |> Draw.move (saucer.position.x, saucer.position.y)

    ExplosionSkin explosion ->
      explosion.fragments
        |> List.map (drawFrag explosion.color)
        |> Draw.group

drawFrag color fragment =
  Draw.circle 3
    |> Draw.filled color
    |> Draw.move (fragment.position.x, fragment.position.y)

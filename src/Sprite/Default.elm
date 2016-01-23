module Sprite.Default where

import Color exposing (..)
import Graphics.Collage exposing (..)

import Model exposing (Game)
import Entity.Ship as Ship exposing (Ship)
import Entity.Rock as Rock exposing (Rock)
import Entity.Saucer as Saucer exposing (Saucer)
import Particle.Explosion as Explosion exposing (Explosion)
import Particle.Bullet as Bullet exposing (Bullet)


type Sprite
  = ShipSprite Ship
  | RockSprite Rock
  | BulletSprite Bullet
  | SaucerSprite Saucer
  | ExplosionSprite Explosion


draw : Game -> List Form
draw game =
  [ List.map RockSprite game.rocks
  , List.map BulletSprite game.bullets
  , List.map SaucerSprite game.saucers
  , List.map ExplosionSprite game.explosions
  ]
    |> List.concat
    |> (::) (ShipSprite game.ship)
    |> List.map drawSingle


drawSingle : Sprite -> Form
drawSingle object =
  case object of

    ShipSprite ship ->
      let
        itime = ship.invulnerableCounter
        flashColor = if itime > 0 then
          if itime % 5 == 0 then white else ship.color1
        else
          ship.color1
        triangle = ngon 3 32
          |> filled flashColor
        engines = rect 4 32
          |> filled ship.color2
          |> move (-18 , 0)
        dline = defaultLine
        bound = circle ship.radius
          |> outlined { dline | color = red }
      in
      if ship.dead then
        circle 0
          |> filled black
      else
        group [ triangle , engines , bound ]
          |> rotate ( degrees (ship.facing + 90 ))
          |> move (ship.position.x, ship.position.y)

    RockSprite rock ->
      let
        body = circle rock.radius
          |> filled rock.color1
        spot1 = circle (rock.radius / 5)
          |> filled rock.color2
          |> move (rock.radius / 3 , rock.radius / 2)
        spot2 = circle (rock.radius / 4)
          |> filled rock.color2
          |> move (rock.radius / -2 , rock.radius / 3.5)
        spot3 = circle (rock.radius / 7)
          |> filled rock.color2
          |> move (rock.radius / -3 , rock.radius / -1.6)
      in
      group [body, spot1, spot2, spot3]
        |> rotate (degrees rock.facing)
        |> move (rock.position.x, rock.position.y)

    BulletSprite bullet ->
      circle bullet.radius
        |> filled bullet.color
        |> move (bullet.position.x, bullet.position.y)

    SaucerSprite saucer ->
      circle saucer.radius
        |> filled darkRed
        |> move (saucer.position.x, saucer.position.y)

    ExplosionSprite explosion ->
      explosion.fragments
        |> List.map (\fragment ->
          circle 3
            |> filled explosion.color
            |> move (fragment.position.x, fragment.position.y)
          )
        |> group

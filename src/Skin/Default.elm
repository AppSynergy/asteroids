module Skin.Default where

import Graphics.Collage as Draw
import Particle.Bullet as Bullet exposing (Bullet)
import Entity.Rock as Rock exposing (Rock)


type Skin
  = RockSkin Rock
  | BulletSkin Bullet


draw : Skin -> Draw.Form
draw object =
  case object of

    RockSkin rock ->
      drawRock rock

    BulletSkin bullet ->
      drawBullet bullet


drawBullet : Bullet -> Draw.Form
drawBullet bullet =
  Draw.circle bullet.radius
    |> Draw.filled bullet.color
    |> Draw.move (bullet.position.x, bullet.position.y)


drawRock : Rock -> Draw.Form
drawRock rock =
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

module Overlay.Message where

import Color
import Graphics.Collage as Draw
import Graphics.Element as Element
import Text

-- MODEL

type alias Message =
  { content : String
  , show : Bool
  }


messageStyle : Text.Style
messageStyle =
  { typeface = [ "Times New Roman", "serif" ]
  , height = Just 80
  , color = Color.lightRed
  , bold = True
  , italic = True
  , line = Nothing
  }


init : String -> Message
init string =
  { content = string
  , show = False
  }


-- UPDATE

update : Bool -> Message -> Message
update b message =
  { message
  | show = b
  }


-- VIEW

draw : Message -> List Draw.Form
draw message =
  if message.show then
    [ textFormat message.content ]
  else
    []


textFormat : String -> Draw.Form
textFormat string =
  string
    |> Text.fromString
    |> Text.style messageStyle
    >> Element.centered
    |> Element.width 150
    >> Draw.toForm

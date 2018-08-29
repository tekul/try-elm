-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/text_fields.html

import Browser
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main =
  Browser.sandbox { init = "", view = view, update = update }


-- UPDATE

type Msg = NewContent String

update (NewContent content) oldContent =
  content


-- VIEW

view content =
  div []
    [ input (myStyle ++ [ placeholder "Text to reverse", onInput NewContent]) []
    , div myStyle [ text (String.reverse content) ]
    ]

myStyle =
    [ style "width" "100%"
    , style "height" "40px"
    , style "padding" "10px 0"
    , style "font-size" "2em"
    , style "text-align" "center"
    ]

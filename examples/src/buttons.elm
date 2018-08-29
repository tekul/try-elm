-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/buttons.html

import Browser
import Html exposing (div, button, text)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, view = view, update = update }


-- MODEL

init = 0


-- UPDATE

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]

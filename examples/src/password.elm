import Browser
import Html exposing (Html, Attribute, text, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main =
  Browser.sandbox { init = "", view = view, update = update }


-- UPDATE

type Msg = NewPassword String

update (NewPassword password) oldPassword =
  password


-- VIEW

view : String -> Html Msg
view password =
  div []
    [ input (myStyle ++ [ type_ "password", placeholder "Password", onInput NewPassword ]) []
    , div myStyle [ text password ]
    ]

myStyle : List (Attribute msg)
myStyle =
    [ style "width" "100%"
    , style "height" "40px"
    , style "padding" "10px 0"
    , style "font-size" "2em"
    , style "text-align" "center"
    ]

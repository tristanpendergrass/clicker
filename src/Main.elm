module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    { thing : String
    }


initialModel : Model
initialModel =
    { thing = "Why hello there"}


type Msg
    = HandleThingInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleThingInput str -> { thing = str }



view : Model -> Html Msg
view model =
    div []
        [ text "Why hello there"
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

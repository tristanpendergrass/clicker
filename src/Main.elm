module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type MainQuestStage
    = One
    | Two
    | Completed


type Module
    = MainQuest MainQuestStage
    | Stockpile
    | GatherWood


type alias Model =
    { modules : List Module
    , wood : Int
    }


initialModel : Model
initialModel =
    { modules = [ GatherWood, Stockpile, MainQuest One ]
    , wood = 0
    }


type Msg
    = HandleGatherWood


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleGatherWood ->
            { model
                | wood = model.wood + 1
            }


renderMainQuest : MainQuestStage -> Html Msg
renderMainQuest stage =
    let
        stageText : String
        stageText =
            case stage of
                One ->
                    "Stage 1 (craft an axe to advance to Stage 2)"

                Two ->
                    "Stage 2 (build a hut to advance to Complete the Game"

                Completed ->
                    "You have completed the game"
    in
    div [ class "main-quest" ]
        [ h2 [ class "module-title" ] [ text "Main Quest" ]
        , div [] [ text stageText ]
        ]


renderStockpile : Int -> Html Msg
renderStockpile wood =
    div [ class "stockpile" ]
        [ h2 [ class "module-title" ] [ text "Stockpile" ]
        , div [] [ text ("Wood: " ++ String.fromInt wood) ]
        ]


renderGatherWood : Html Msg
renderGatherWood =
    div [ class "gather-wood" ]
        [ h2 [ class "module-title" ] [ text "Gather Wood" ]
        , button [ onClick HandleGatherWood ] [ text "Forage for sticks" ]
        ]


renderModule : Model -> Module -> Html Msg
renderModule model mod =
    let
        inner : Html Msg
        inner =
            case mod of
                MainQuest stage ->
                    renderMainQuest stage

                Stockpile ->
                    renderStockpile model.wood

                GatherWood ->
                    renderGatherWood
    in
    div [ class "module" ] [ inner ]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "modules-container" ]
            (List.map (renderModule model) <| List.reverse model.modules)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

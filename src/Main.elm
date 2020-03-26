module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)


type MainQuestStage
    = One
    | Two
    | Completed


type Module
    = MainQuest
    | Stockpile
    | GatherWood
    | ConstructAxe
    | BuildHut


type alias Model =
    { modules : List Module
    , mainQuestStage : MainQuestStage
    , wood : Int
    , hasAxe : Bool
    }


initialModel : Model
initialModel =
    { modules =
        List.reverse
            [ MainQuest
            , Stockpile
            , GatherWood
            , ConstructAxe
            ]
    , mainQuestStage = One
    , wood = 0
    , hasAxe = False
    }


type Msg
    = HandleGatherWood
    | HandleConstructAxe
    | HandleBuildHut


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleGatherWood ->
            let
                amountGathered : Int
                amountGathered =
                    if model.hasAxe then
                        10

                    else
                        1
            in
            { model
                | wood = model.wood + amountGathered
            }

        HandleConstructAxe ->
            let
                newModules : List Module
                newModules =
                    model.modules
                        |> List.filter (\mod -> mod /= ConstructAxe)
                        |> (::) BuildHut
            in
            if model.wood >= 10 then
                { model
                    | wood = model.wood - 10
                    , hasAxe = True
                    , modules = newModules
                    , mainQuestStage = Two
                }

            else
                model

        HandleBuildHut ->
            let
                newModules : List Module
                newModules =
                    model.modules
                        |> List.filter (\mod -> mod /= BuildHut)
            in
            if model.wood >= 10 then
                { model
                    | wood = model.wood - 100
                    , modules = newModules
                    , mainQuestStage = Completed
                }

            else
                model


renderMainQuest : MainQuestStage -> Html Msg
renderMainQuest stage =
    let
        stageText : String
        stageText =
            case stage of
                One ->
                    "Stage 1 (craft an axe to advance to Stage 2)"

                Two ->
                    "Stage 2 (build a hut to Complete the Game"

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


renderGatherWood : Bool -> Html Msg
renderGatherWood hasAxe =
    div [ class "gather-wood" ]
        [ h2 [ class "module-title" ] [ text "Gather Wood" ]
        , button [ onClick HandleGatherWood ]
            [ text
                (if hasAxe then
                    "Chop Wood"

                 else
                    "Forage for sticks"
                )
            ]
        ]


renderConstructAxe : Int -> Html Msg
renderConstructAxe wood =
    div [ class "construct-axe" ]
        [ h2 [ class "module-title" ] [ text "Construct Axe" ]
        , button [ onClick HandleConstructAxe, disabled (wood < 10) ] [ text "Construct for 10 Wood" ]
        ]


renderBuildHut : Int -> Html Msg
renderBuildHut wood =
    div [ class "build-hut" ]
        [ h2 [ class "module-title" ] [ text "Build Hut" ]
        , button [ onClick HandleBuildHut, disabled (wood < 100) ] [ text "Build for 100 Wood" ]
        ]


renderModule : Model -> Module -> Html Msg
renderModule model mod =
    let
        inner : Html Msg
        inner =
            case mod of
                MainQuest ->
                    renderMainQuest model.mainQuestStage

                Stockpile ->
                    renderStockpile model.wood

                GatherWood ->
                    renderGatherWood model.hasAxe

                ConstructAxe ->
                    renderConstructAxe model.wood

                BuildHut ->
                    renderBuildHut model.wood
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

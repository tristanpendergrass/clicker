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
    | UpgradeAxe
    | BuildHut


type AxeStatus
    = AxeUnowned
    | AxeLevel1
    | AxeLevel2


type alias Model =
    { modules : List Module
    , mainQuestStage : MainQuestStage
    , wood : Int
    , axeStatus : AxeStatus
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
    , axeStatus = AxeUnowned
    }


type Msg
    = HandleGatherWood
    | HandleConstructAxe
    | HandleBuildHut
    | HandleUpgradeAxe


update : Msg -> Model -> Model
update msg model =
    case msg of
        HandleGatherWood ->
            let
                amountGathered : Int
                amountGathered =
                    case model.axeStatus of
                        AxeUnowned ->
                            1

                        AxeLevel1 ->
                            5

                        AxeLevel2 ->
                            10
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
                        |> (::) UpgradeAxe
                        |> (::) BuildHut
            in
            if model.wood >= 10 then
                { model
                    | wood = model.wood - 10
                    , axeStatus = AxeLevel1
                    , modules = newModules
                    , mainQuestStage = Two
                }

            else
                model

        HandleUpgradeAxe ->
            let
                newModules : List Module
                newModules =
                    model.modules |> List.filter (\mod -> mod /= UpgradeAxe)
            in
            if model.wood >= 25 then
                { model | wood = model.wood - 25, modules = newModules, axeStatus = AxeLevel2 }

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
    div []
        [ h2 [ class "module-title" ] [ text "Main Quest" ]
        , div [] [ text stageText ]
        ]


renderStockpile : Int -> Html Msg
renderStockpile wood =
    div [ class "stockpile" ]
        [ h2 [ class "module-title" ] [ text "Stockpile" ]
        , div [] [ text ("Wood: " ++ String.fromInt wood) ]
        ]


renderGatherWood : AxeStatus -> Html Msg
renderGatherWood axeStatus =
    let
        chopText : String
        chopText =
            case axeStatus of
                AxeUnowned ->
                    "Forage for sticks (1 wood)"

                AxeLevel1 ->
                    "Chop Wood (5 wood)"

                AxeLevel2 ->
                    "Chop Wood (10 wood)"
    in
    div []
        [ h2 [ class "module-title" ] [ text "Gather Wood" ]
        , button [ onClick HandleGatherWood ] [ text chopText ]
        ]


renderConstructAxe : Int -> Html Msg
renderConstructAxe wood =
    div []
        [ h2 [ class "module-title" ] [ text "Construct Axe" ]
        , button [ onClick HandleConstructAxe, disabled (wood < 10) ] [ text "Construct for 10 Wood" ]
        ]


renderUpgradeAxe : Int -> Html Msg
renderUpgradeAxe wood =
    div []
        [ h2 [ class "module-title" ] [ text "Upgrade Axe" ]
        , button [ onClick HandleUpgradeAxe, disabled (wood < 25) ] [ text "Upgrade for 25 Wood" ]
        ]


renderBuildHut : Int -> Html Msg
renderBuildHut wood =
    div []
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
                    renderGatherWood model.axeStatus

                ConstructAxe ->
                    renderConstructAxe model.wood

                UpgradeAxe ->
                    renderUpgradeAxe model.wood

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

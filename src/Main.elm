module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Module
    = Store
    | MoneyPrinter


type alias Model =
    { modules : List Module
    , money : Int
    }


initialModel : Model
initialModel =
    { modules = [ Store, MoneyPrinter ]
    , money = 0
    }


type Msg
    = PurchaseMoneyPrinter
    | HandleMoneyPrinterClick


update : Msg -> Model -> Model
update msg model =
    case msg of
        PurchaseMoneyPrinter ->
            { model
                | modules = MoneyPrinter :: model.modules
                , money = model.money - 5
            }

        HandleMoneyPrinterClick ->
            { model | money = model.money + 1 }


renderStore : Html Msg
renderStore =
    div [ class "store" ]
        [ h2 [ class "module-title" ] [ text "Store" ]
        , button [ onClick PurchaseMoneyPrinter ] [ text "(5) Purchase Money Printer" ]
        ]


renderMoneyPrinter : Html Msg
renderMoneyPrinter =
    div [ class "money-printer" ]
        [ h2 [ class "module-title" ] [ text "Money Printer" ]
        , button [ onClick HandleMoneyPrinterClick ] [ text "Print" ]
        ]


renderModule : Module -> Html Msg
renderModule mod =
    let
        inner : Html Msg
        inner =
            case mod of
                Store ->
                    renderStore

                MoneyPrinter ->
                    renderMoneyPrinter
    in
    div [ class "module" ] [ inner ]


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "money" ] [ h1 [] [ text (String.fromInt model.money) ] ]
        , hr [] []
        , div [ class "modules-container" ]
            (List.map renderModule model.modules)
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

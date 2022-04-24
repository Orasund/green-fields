module Pages.Mine exposing (Model, Msg, page)

import Config
import Data.DiceBag as DiceBag
import Data.Die as Die exposing (Die)
import Effect exposing (Effect)
import Gen.Params.Mine exposing (Params)
import Html.Styled as Html exposing (Html)
import Page
import Request
import Shared
import View exposing (View)
import View.NoDice as NoDice
import View.Style as Style


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = Mine Die { amount : Int }


price : Int -> Int
price amount =
    2
        ^ amount
        * Config.priceOfStone


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Mine die { amount } ->
            ( model
            , [ [ ( die, amount ) ]
                    |> DiceBag.fromList
                    |> Shared.RemoveDice
                    |> Effect.fromShared
              , amount
                    |> price
                    |> Shared.AddMoney
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewOre : ( Die, Int ) -> Html Msg
viewOre ( die, amount ) =
    [ "Use " |> Html.text
    , [ ( die, amount ) ]
        |> DiceBag.fromList
        |> DiceBag.toString
        |> Html.text
    , " to " |> Html.text
    , Style.button "Mine"
        (Mine die { amount = amount } |> Just)
    , " and get "
        ++ (price amount |> String.fromInt)
        ++ Config.moneySymbol
        ++ "."
        |> Html.text
    ]
        |> Style.row


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Mine"
    , body =
        if DiceBag.isEmpty shared.dice then
            [ NoDice.view ]

        else
            case
                shared.dice
                    |> DiceBag.toList
                    |> List.filter (\( i, n ) -> n > 1)
                    |> List.map viewOre
            of
                [] ->
                    [ "You did not have any luck in mines today. "
                        ++ "Come back once you have a double or a tiple die."
                        |> Style.paragraph
                    ]

                list ->
                    [ Style.section "Mining Ore"
                        list
                    ]
    }

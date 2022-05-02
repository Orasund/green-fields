module Pages.Mine exposing (Model, Msg, page)

import Config
import Data.AnyBag as AnyBag exposing (AnyBag)
import Data.DiceBag as DiceBag
import Data.Die as Die exposing (Die)
import Data.Food as Food
import Data.Stone as Stone
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
    = Mine Die { amount : Int } Stone.Stone
    | Sell Stone.Stone


price : Int -> Int
price amount =
    2
        ^ amount
        * Config.priceOfStone


viewStone : AnyBag String Stone.Stone -> Html Msg
viewStone list =
    list
        |> AnyBag.toList
        |> List.map
            (\( item, amount ) ->
                [ Stone.toString item |> Html.text
                , String.fromInt amount ++ "x" |> Html.text
                , Stone.description item |> Html.text
                , Just (Sell item)
                    |> Style.button ("Sell for " ++ (Stone.price item |> String.fromInt) ++ Config.moneySymbol)
                ]
            )
        |> Style.table [ "Name", "Amount", "Effect", "Sell" ]


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Sell food ->
            ( model
            , [ food |> Stone.price |> Shared.AddMoney |> Effect.fromShared
              , food |> Shared.AddStone -1 |> Effect.fromShared
              ]
                |> Effect.batch
            )

        Mine die { amount } stone ->
            ( model
            , [ [ ( die, amount ) ]
                    |> DiceBag.fromList
                    |> Shared.RemoveDice
                    |> Effect.fromShared
              , stone
                    |> Shared.AddStone 1
                    |> Effect.fromShared
              ]
                |> List.map identity
                |> Effect.batch
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewOre : ( Die, Int ) -> Html Msg
viewOre ( die, int ) =
    let
        stone =
            int |> Stone.fromInt |> Maybe.withDefault Stone.last
    in
    [ "Use " |> Html.text
    , [ ( die, int ) ]
        |> DiceBag.fromList
        |> DiceBag.toString
        |> Html.text
    , " to " |> Html.text
    , Style.button "Mine"
        (Mine die { amount = int } stone |> Just)
    , " and get one "
        ++ (stone |> Stone.toEmoji)
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
            (case
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
            )
                ++ [ viewStone shared.stone ]
    }

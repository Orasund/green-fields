module Pages.Fields exposing (Model, Msg, page)

import Array exposing (Array)
import Config
import Css
import Data.AnyBag as AnyBag
import Data.DiceBag as DiceBag
import Data.Die as Dice exposing (Die)
import Data.Food as Food exposing (Food(..))
import Data.Food.Vegetable as Vegetable exposing (Vegetable(..))
import Effect exposing (Effect)
import Gen.Params.Fields exposing (Params)
import Gen.Route as Route
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
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
    = Plant Int Vegetable
    | BuyField { price : Int }
    | Remove Int
    | Harvest Vegetable


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        BuyField { price } ->
            ( model
            , [ -price
                    |> Shared.AddMoney
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )

        Plant i vegi ->
            ( model
            , [ vegi
                    |> Vegetable.toDie
                    |> (\die -> [ ( die, 1 ) ])
                    |> DiceBag.fromList
                    |> Shared.RemoveDice
                    |> Effect.fromShared
              , vegi
                    |> Shared.Plant i
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )

        Remove i ->
            ( model
            , Shared.RemovePlant i
                |> Effect.fromShared
            )

        Harvest vegi ->
            ( model
            , [ vegi
                    |> Vegetable.toDie
                    |> (\die -> [ ( die, 1 ) ])
                    |> DiceBag.fromList
                    |> Shared.RemoveDice
                    |> Effect.fromShared
              , vegi
                    |> VegetableFood
                    |> Shared.AddItem
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewField : Shared.Model -> ( Int, Maybe Vegetable ) -> Html Msg
viewField shared ( i, maybeVege ) =
    case maybeVege of
        Just vege ->
            let
                name =
                    VegetableFood vege |> Food.toString

                die =
                    vege |> Vegetable.toDie
            in
            Style.section (name ++ " Field")
                [ "Use a "
                    ++ (die |> Dice.toString)
                    ++ " to get a "
                    ++ name
                    |> Style.paragraph
                , [ Style.button "Harvest"
                        (if shared.dice |> DiceBag.member die then
                            Just (Harvest vege)

                         else
                            Nothing
                        )
                  , Html.text " "
                  , Just (Remove i)
                        |> Style.button "Remove"
                  ]
                    |> Style.row
                ]

        Nothing ->
            Style.section "Empty Field"
                [ Style.paragraph "Use a single die to plant some vegetables"
                , Vegetable.asList
                    |> List.filterMap
                        (\vege ->
                            let
                                name =
                                    VegetableFood vege |> Food.toString

                                die =
                                    vege |> Vegetable.toDie
                            in
                            if shared.dice |> DiceBag.member die then
                                Just
                                    [ name ++ " Farm" |> Html.text
                                    , die |> Dice.toString |> Html.text
                                    , Style.button "Build" (Just (Plant i vege))
                                    ]

                            else
                                Nothing
                        )
                    |> Style.table [ "Build", "Cost", "Action" ]
                ]


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Fields"
    , body =
        if DiceBag.isEmpty shared.dice then
            [ NoDice.view ]

        else
            let
                price =
                    Config.fieldBasePrice
            in
            [ Style.button ("Buy another Field for " ++ String.fromInt price ++ Config.moneySymbol ++ ".")
                (if shared.money >= price then
                    BuyField { price = price } |> Just

                 else
                    Nothing
                )
            , shared.fields
                |> Array.toIndexedList
                |> List.map (viewField shared)
                |> Html.div []
            ]
    }

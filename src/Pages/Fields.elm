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
                [ [ [ Style.button ("Harvest 1 " ++ name)
                        (if shared.dice |> DiceBag.member die then
                            Just (Harvest vege)

                         else
                            Nothing
                        )
                    , " for " ++ (die |> Dice.toString) |> Html.text
                    ]
                        |> Style.row
                  , [ "You can also " |> Html.text
                    , Just (Remove i)
                        |> Style.button "Remove"
                    , " it for free to get back an empty field." |> Html.text
                    ]
                        |> Style.row
                  ]
                    |> List.intersperse (Html.text " ")
                    |> Style.row
                ]

        Nothing ->
            Style.section "Empty Field"
                [ Style.paragraph "Choose the vegetable you want to plant here"
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
                                [ name ++ " Farm" |> Style.bold
                                , "Harvest 1 " ++ name ++ " for " ++ (die |> Dice.toString) |> Html.text
                                , Style.button ("Build for " ++ (die |> Dice.toString))
                                    (Just (Plant i vege))
                                ]
                                    |> Style.column
                                    |> Just

                            else
                                Nothing
                        )
                    |> Style.list
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
            [ [ Style.button "Buy another Field"
                    (if shared.money >= price then
                        BuyField { price = price } |> Just

                     else
                        Nothing
                    )
              , " for "
                    ++ String.fromInt price
                    ++ Config.moneySymbol
                    ++ "."
                    |> Html.text
              ]
                |> Style.row
            , shared.fields
                |> Array.toIndexedList
                |> List.map (viewField shared)
                |> Html.div []
            ]
    }

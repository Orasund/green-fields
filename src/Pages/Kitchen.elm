module Pages.Kitchen exposing (Model, Msg, page)

import Config
import Css
import Data.AnyBag as AnyBag
import Data.DiceBag as DiceBag exposing (DiceBag)
import Data.Die as Dice exposing (Die)
import Data.Food as Food exposing (Food(..))
import Data.Food.SeaFood as Fish exposing (SeaFood)
import Data.Food.Vegetable as Vegetable exposing (Vegetable)
import Effect exposing (Effect)
import Gen.Params.Kitchen exposing (Params)
import Gen.Route as Route
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Page
import Request
import Shared
import View exposing (View)
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
    = Rethrow
    | Sell Food
    | ApplyFish SeaFood
    | ApplyVegetable Int Die Vegetable


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Rethrow ->
            ( model, Shared.Rethrow |> Effect.fromShared )

        Sell food ->
            ( model
            , [ food |> Food.price |> Shared.AddMoney |> Effect.fromShared
              , food |> Shared.RemoveFood 1 |> Effect.fromShared
              ]
                |> Effect.batch
            )

        ApplyFish fish ->
            ( model
            , [ SeaFood fish |> Shared.RemoveFood 1 |> Effect.fromShared
              , fish
                    |> Fish.toAmount
                    |> Shared.AddRandomDice
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )

        ApplyVegetable amount die vegetable ->
            let
                maybeNewDie =
                    die
                        |> Dice.add (Vegetable.modifier vegetable)
            in
            case maybeNewDie of
                Just newDie ->
                    ( model
                    , [ VegetableFood vegetable |> Shared.RemoveFood amount |> Effect.fromShared
                      , [ ( die, 1 ) ]
                            |> DiceBag.fromList
                            |> Shared.RemoveDice
                            |> Effect.fromShared
                      , [ ( newDie, 1 ) ]
                            |> DiceBag.fromList
                            |> Shared.AddDice
                            |> Effect.fromShared
                      ]
                        |> Effect.batch
                    )

                Nothing ->
                    ( model, Effect.none )



-- VIEW


viewFood : AnyBag.AnyBag String Food -> Html Msg
viewFood list =
    list
        |> AnyBag.toList
        |> List.map
            (\( item, amount ) ->
                [ Food.toString item |> Html.text
                , String.fromInt amount ++ "x" |> Html.text
                , Food.description item |> Html.text
                , Just (Sell item)
                    |> Style.button ("Sell for " ++ (Food.price item |> String.fromInt) ++ Config.moneySymbol)
                ]
            )
        |> Style.table [ "Name", "Amount", "Effect", "Sell" ]


viewDie : Shared.Model -> ( Die, Int ) -> Html Msg
viewDie shared ( die, amount ) =
    (Vegetable.asList
        |> List.concatMap
            (\vegetable ->
                let
                    modifier =
                        vegetable
                            |> Vegetable.modifier

                    count =
                        shared.food |> AnyBag.count (VegetableFood vegetable)
                in
                if count > 0 then
                    List.range 1 (min count 5)
                        |> List.filterMap
                            (\multiplier ->
                                die
                                    |> Dice.add (modifier * multiplier)
                                    |> Maybe.map
                                        (\newDie ->
                                            [ Style.button ("Change into " ++ Dice.toString newDie)
                                                (Just (ApplyVegetable multiplier die vegetable))
                                            , " using " ++ String.fromInt multiplier ++ Vegetable.toString vegetable |> Html.text
                                            ]
                                                |> Html.div []
                                        )
                            )

                else
                    []
            )
    )
        |> (\list ->
                if List.isEmpty list then
                    Style.none

                else
                    ("Modify "
                        ++ (die
                                |> Dice.toString
                                |> List.repeat amount
                                |> String.concat
                           )
                        |> Style.bold
                    )
                        :: list
                        |> Style.column
           )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Kitchen"
    , body =
        [ if DiceBag.isEmpty shared.dice then
            [ Style.button "Roll the dice" (Just Rethrow)
            , " to start the next turn." |> Html.text
            ]
                |> Style.row

          else
            [ [ "You can use vegetables from the " |> Html.text
              , Style.link Route.Fields
              , " to modify your die. " |> Html.text
              ]
                |> Style.row
            , if shared.dice |> DiceBag.streets |> List.isEmpty then
                Style.none

              else
                [ "Head over to the " |> Html.text
                , Style.link Route.Lake
                , " to catch a fish. Fish give you extra die." |> Html.text
                ]
                    |> Style.row
            , if shared.dice |> DiceBag.toList |> List.filter (\( _, n ) -> n >= 2) |> List.isEmpty then
                Style.none

              else
                [ "Head over to the " |> Html.text
                , Style.link Route.Mine
                , " to mine some ore and sell it for good money." |> Html.text
                ]
                    |> Style.row
            , shared.dice
                |> DiceBag.toList
                |> List.map (viewDie shared)
                |> Style.filledRow
            , [ "To end your turn, go to the " |> Html.text
              , Style.link Route.Woods
              , " and chop some wood." |> Html.text
              ]
                |> Style.row
            , Style.paragraph " "
            , Fish.asList
                |> List.filterMap
                    (\fish ->
                        let
                            amount =
                                fish |> Fish.toAmount
                        in
                        if (amount > 0) && (shared.food |> AnyBag.member (SeaFood fish)) then
                            Style.button
                                ("+"
                                    ++ String.fromInt amount
                                )
                                (Just (ApplyFish fish))
                                |> Just

                        else
                            Nothing
                    )
                |> (\list ->
                        if List.isEmpty list then
                            []

                        else
                            Style.bold "Add Dice:" :: list
                   )
                |> List.intersperse (Html.text " ")
                |> Style.row
            ]
                |> Style.section ("Your Dice: " ++ (shared.dice |> DiceBag.toString))
        , if AnyBag.isEmpty shared.food then
            Style.none

          else
            Style.section "Food"
                [ shared.food
                    |> viewFood
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

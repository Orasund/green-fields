module Pages.Kitchen exposing (Model, Msg, page)

import Config
import Css
import Data.AnyBag as AnyBag
import Data.DiceBag as DiceBag
import Data.Die as Dice exposing (Die)
import Data.Food as Food exposing (Food(..))
import Data.Food.Fish as Fish exposing (Fish)
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
    | ApplyFish Fish
    | ApplyVegetable Die Vegetable


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Rethrow ->
            ( model, Shared.Rethrow |> Effect.fromShared )

        Sell food ->
            ( model
            , [ food |> Food.price |> Shared.AddMoney |> Effect.fromShared
              , food |> Shared.RemoveItem |> Effect.fromShared
              ]
                |> Effect.batch
            )

        ApplyFish fish ->
            ( model
            , [ FishFood fish |> Shared.RemoveItem |> Effect.fromShared
              , fish
                    |> Fish.toAmount
                    |> Shared.AddRandomDice
                    |> Effect.fromShared
              ]
                |> Effect.batch
            )

        ApplyVegetable die vegetable ->
            let
                maybeNewDie =
                    die
                        |> Dice.add (Vegetable.modifier vegetable)
            in
            case maybeNewDie of
                Just newDie ->
                    ( model
                    , [ VegetableFood vegetable |> Shared.RemoveItem |> Effect.fromShared
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


viewItems : AnyBag.AnyBag String Food -> Html Msg
viewItems list =
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
    [ die
        |> Dice.toString
        |> List.repeat amount
        |> String.concat
        |> Html.text
        |> List.singleton
        |> Html.div [ Attr.css [ Css.textAlign Css.center, Css.fontSize Css.xLarge ] ]
    , ((Vegetable.asList
            |> List.filterMap
                (\vegetable ->
                    let
                        modifier =
                            vegetable
                                |> Vegetable.modifier
                    in
                    if shared.items |> AnyBag.member (VegetableFood vegetable) then
                        die
                            |> Dice.add modifier
                            |> Maybe.map
                                (\_ ->
                                    Style.button
                                        (modifier
                                            |> (\int ->
                                                    if int > 0 then
                                                        "+" ++ String.fromInt int

                                                    else
                                                        String.fromInt int
                                               )
                                        )
                                        (Just (ApplyVegetable die vegetable))
                                )

                    else
                        Nothing
                )
       )
        |> (\list ->
                if List.isEmpty list then
                    []

                else
                    ("Modify:" |> Style.bold) :: list
           )
        |> List.intersperse (Html.text " ")
      )
        |> Style.row
    ]
        |> Html.div []


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Kitchen"
    , body =
        [ (if DiceBag.isEmpty shared.dice then
            [ [ Style.button "Roll the dice" (Just Rethrow)
              , " to start the next turn." |> Html.text
              ]
                |> Style.row
            ]

           else
            [ "You can use food to modify die and add more dice. "
                |> Style.paragraph
            , shared.dice
                |> DiceBag.toList
                |> List.map (viewDie shared)
                |> Style.filledRow
            , [ "To end your turn, go to the " |> Html.text
              , Style.link "woods" Route.Woods
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
                        if shared.items |> AnyBag.member (FishFood fish) then
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
          )
            |> Style.section "Your Dice"
        , Style.section "Food"
            [ shared.items
                |> viewItems
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

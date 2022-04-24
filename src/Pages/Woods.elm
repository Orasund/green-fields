module Pages.Woods exposing (Model, Msg, page)

import Config
import Data.DiceBag as DiceBag exposing (DiceBag)
import Data.Die as Dice exposing (Die)
import Effect exposing (Effect)
import Gen.Params.Woods exposing (Params)
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
    { selectedDice : DiceBag }


init : ( Model, Effect Msg )
init =
    ( { selectedDice = DiceBag.empty }, Effect.none )



-- UPDATE


type Msg
    = SelectDice Int Die
    | SelectAllAndChop DiceBag
    | UnselectDie Die
    | ChopWood


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SelectDice n die ->
            ( { model
                | selectedDice =
                    model.selectedDice
                        |> DiceBag.addN n die
              }
            , Effect.none
            )

        SelectAllAndChop selectedDice ->
            { model
                | selectedDice = selectedDice
            }
                |> update ChopWood

        UnselectDie die ->
            ( { model | selectedDice = model.selectedDice |> DiceBag.remove die }, Effect.none )

        ChopWood ->
            ( { model | selectedDice = DiceBag.empty }
            , Effect.batch
                [ Shared.RemoveDice model.selectedDice |> Effect.fromShared
                , model.selectedDice
                    |> DiceBag.size
                    |> (*) Config.priceOfWood
                    |> Shared.AddMoney
                    |> Effect.fromShared
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewSelection : Shared.Model -> DiceBag -> Html Msg
viewSelection shared selectedDice =
    [ [ "Remaining", "Selected" ]
        |> List.map
            (\string ->
                string |> Html.text |> Style.elem
            )
        |> Style.filledRow
    , shared.dice
        |> DiceBag.toList
        |> List.map
            (\( die, n ) ->
                let
                    selected =
                        selectedDice |> DiceBag.count die
                in
                [ List.repeat (n - selected) (die |> Dice.toString)
                    |> String.concat
                    |> Html.text
                    |> Style.elem
                , [ List.repeat selected (die |> Dice.toString)
                        |> String.concat
                        |> Html.text
                  , Style.button "-"
                        (if selected > 0 then
                            Just (UnselectDie die)

                         else
                            Nothing
                        )
                  , selected |> String.fromInt |> Html.text
                  , Style.button "+"
                        (if selected < n then
                            Just (SelectDice 1 die)

                         else
                            Nothing
                        )
                  ]
                    |> List.intersperse (Html.text " ")
                    |> Style.row
                ]
                    |> Style.filledRow
            )
        |> Style.row
    ]
        |> Html.div []


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Woods"
    , body =
        if DiceBag.isEmpty shared.dice then
            [ NoDice.view ]

        else
            [ [ "You can use all leftover dice to " |> Html.text
              , Style.button "Chop wood"
                    (if model.selectedDice == shared.dice then
                        Nothing

                     else
                        Just (SelectAllAndChop shared.dice)
                    )
              , " and gain "
                    ++ (Config.priceOfWood * DiceBag.size shared.dice |> String.fromInt)
                    ++ Config.moneySymbol
                    ++ "."
                    |> Html.text
              ]
                |> Style.row
            ]
    }

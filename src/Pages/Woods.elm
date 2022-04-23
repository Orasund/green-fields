module Pages.Woods exposing (Model, Msg, page)

import Config
import Data.Dice as Dice exposing (Dice)
import Data.DiceBag as DiceBag exposing (DiceBag)
import Effect exposing (Effect)
import Gen.Params.Woods exposing (Params)
import Html.Styled as Html
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
    { selectedDice : DiceBag }


init : ( Model, Effect Msg )
init =
    ( { selectedDice = DiceBag.empty }, Effect.none )



-- UPDATE


type Msg
    = SelectDie Dice
    | SelectN Int Dice
    | UnselectDie Dice
    | ChopWood


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SelectDie die ->
            ( { model | selectedDice = model.selectedDice |> DiceBag.add die }, Effect.none )

        SelectN n die ->
            ( { model
                | selectedDice =
                    model.selectedDice
                        |> DiceBag.addN n die
              }
            , Effect.none
            )

        UnselectDie die ->
            ( { model | selectedDice = model.selectedDice |> DiceBag.remove die }, Effect.none )

        ChopWood ->
            ( { model | selectedDice = DiceBag.empty }
            , Effect.batch
                [ Shared.RemoveDice model.selectedDice |> Effect.fromShared
                , model.selectedDice
                    |> DiceBag.size
                    |> (*) 2
                    |> Shared.AddMoney
                    |> Effect.fromShared
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Woods"
    , body =
        [ "You can use leftover die to choop some wood." |> Style.paragraph
        , Style.section "Choop Wood"
            [ "Select the dice you want to use for chopping."
                |> Style.paragraph
            , shared.dice
                |> DiceBag.toList
                |> List.map
                    (\( die, n ) ->
                        let
                            selected =
                                model.selectedDice |> DiceBag.count die
                        in
                        [ List.repeat (n - selected) (die |> Dice.toString)
                            |> String.concat
                            |> Html.text
                            |> List.singleton
                            |> Html.div []
                        , [ List.repeat selected (die |> Dice.toString)
                                |> String.concat
                                |> Html.text
                                |> List.singleton
                                |> Html.div []
                          , [ Style.button "-"
                                (if selected > 0 then
                                    Just (UnselectDie die)

                                 else
                                    Nothing
                                )
                            , Html.text " "
                            , selected |> String.fromInt |> Html.text
                            , Html.text " "
                            , Style.button "+"
                                (if selected < n then
                                    Just (SelectDie die)

                                 else
                                    Nothing
                                )
                            , Html.text " "
                            , Style.button "++"
                                (if n > 1 && selected < n then
                                    Just (SelectN (n - selected) die)

                                 else
                                    Nothing
                                )
                            ]
                                |> Html.div []
                          ]
                            |> Style.filledRow
                        ]
                    )
                |> Style.table [ "Remaining", "Selected" ]
            , Style.paragraph ""
            , Style.button "Chop Wood"
                (if DiceBag.isEmpty model.selectedDice then
                    Nothing

                 else
                    Just ChopWood
                )
            ]
        ]
    }

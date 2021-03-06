module Pages.Lake exposing (Model, Msg, page)

import Config
import Data.AnyBag as AnyBag
import Data.DiceBag as DiceBag exposing (DiceBag)
import Data.Die as Dice exposing (Die)
import Data.Food as Food
import Data.Food.SeaFood as SeaFood
import Data.Stone as Stone
import Effect exposing (Effect)
import Gen.Enum.SeaFood exposing (SeaFood)
import Gen.Enum.Stone exposing (Stone)
import Gen.Params.Lake exposing (Params)
import Html.Styled as Html exposing (Html)
import List.Extra
import Page
import Random
import Random.List
import Request
import Shared exposing (Msg(..))
import StaticArray.Index as Index
import Svg.Styled.Attributes exposing (result)
import View exposing (View)
import View.NoDice as NoDice
import View.Style as Style


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update shared
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
    = Catch (List Die)
    | AddSeaFood SeaFood Stone


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        Catch dice ->
            shared.fishingPool
                |> (\( head, tail ) -> head :: tail)
                |> Random.List.choose
                |> Random.map
                    (\( maybe, list ) ->
                        ( maybe |> Maybe.withDefault SeaFood.first
                        , case list of
                            [] ->
                                ( SeaFood.first, [] )

                            head :: tail ->
                                ( head, tail )
                        )
                    )
                |> (\generator -> Random.step generator shared.seed)
                |> (\( ( seaFood, list ), seed ) ->
                        ( model
                        , [ Shared.SetSeed seed
                          , Shared.SetFishingPool list
                          , Shared.AddFood (Food.SeaFood seaFood)
                          , dice
                                |> List.map (\die -> ( die, 1 ))
                                |> DiceBag.fromList
                                |> Shared.AddDice
                          ]
                            |> List.map Effect.fromShared
                            |> Effect.batch
                        )
                   )

        AddSeaFood seaFood stone ->
            ( model
            , [ Shared.AddStone -1 stone |> Effect.fromShared
              , Shared.AddToFishingPool seaFood |> Effect.fromShared
              ]
                |> Effect.batch
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewStreet : List Die -> Html Msg
viewStreet list =
    list
        |> List.length
        |> SeaFood.fromStreet
        |> (\fish ->
                let
                    name =
                        fish |> SeaFood.toString
                in
                Style.section ("Catch " ++ name)
                    [ [ "Use "
                            ++ (list
                                    |> List.map Dice.toString
                                    |> String.concat
                               )
                            ++ " go "
                            |> Html.text
                      , Catch list |> Just |> Style.button "fishing"
                      , "." |> Html.text
                      ]
                        |> Style.row
                    ]
           )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Lake"
    , body =
        let
            odds =
                shared.fishingPool
                    |> (\( head, tail ) -> head :: tail)
                    |> List.Extra.gatherEquals
                    |> List.map
                        (Tuple.mapSecond
                            (\list ->
                                (toFloat (list |> List.length) + 1)
                                    * 100
                                    / (shared.fishingPool
                                        |> (\( head, tail ) -> head :: tail)
                                        |> List.length
                                        |> toFloat
                                      )
                                    |> round
                            )
                        )
        in
        [ "Pool: "
            ++ (shared.fishingPool
                    |> (\( head, tail ) -> head :: tail)
                    |> List.map SeaFood.toEmoji
                    |> String.concat
               )
            |> Style.paragraph
        , odds
            |> List.map (\( seaFood, odd ) -> String.fromInt odd ++ "% to catch a " ++ SeaFood.toEmoji seaFood)
            |> String.join ", "
            |> (\string -> "There is a change of " ++ string ++ ".")
            |> Style.paragraph
        ]
            ++ (shared.stone
                    |> AnyBag.toList
                    |> List.filterMap
                        (\( stone, _ ) ->
                            stone
                                |> Stone.toInt
                                |> (+) 1
                                |> SeaFood.fromInt
                                |> Maybe.map
                                    (\seaFood ->
                                        AddSeaFood seaFood stone
                                            |> Just
                                            |> Style.button ("+1 " ++ SeaFood.toEmoji seaFood ++ " for " ++ Stone.toEmoji stone)
                                    )
                        )
               )
            ++ (if DiceBag.isEmpty shared.dice then
                    [ NoDice.view ]

                else
                    case
                        shared.dice
                            |> DiceBag.streets
                    of
                        [] ->
                            [ "You could not catch a fish today. "
                                ++ "Come back once you have a street of 3 or more die to catch some fish."
                                |> Style.paragraph
                            ]

                        list ->
                            list |> List.map viewStreet
               )
    }

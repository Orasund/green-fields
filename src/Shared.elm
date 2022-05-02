module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Array exposing (Array)
import Data.AnyBag as AnyBag exposing (AnyBag)
import Data.DiceBag as Dicebag exposing (DiceBag)
import Data.Die as Dice
import Data.Food as Food exposing (Food)
import Data.Food.SeaFood as SeaFood
import Data.Food.Vegetable as Vegetable exposing (Vegetable)
import Data.Stone
import Gen.Record.Shared as Shared
import Json.Decode as Json
import Random exposing (Generator, Seed)
import Random.List
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    Shared.Shared


type Msg
    = NoOp
    | GotSeed Seed
    | Fish SeaFood.SeaFood
    | AddToFishingPool SeaFood.SeaFood
    | AddSeaFood
    | AddFood Food
    | RemoveFood Int Food
    | AddStone Int Data.Stone.Stone
    | AddField
    | Plant Int Vegetable
    | RemovePlant Int
    | AddMoney Int
    | AddRandomDice Int
    | AddDice DiceBag
    | RemoveDice DiceBag
    | Rethrow


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { dice = Dicebag.empty
      , money = 0
      , seed = Random.initialSeed 42
      , food = AnyBag.empty Food.toString
      , stone = AnyBag.empty Data.Stone.toString
      , fields = Array.fromList [ Nothing ]
      , fishingPool = ( SeaFood.default, [] )
      }
    , Random.independentSeed |> Random.generate GotSeed
    )


applyGenerator : Seed -> Generator Model -> Model
applyGenerator seed generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update request msg model =
    let
        noCmd m =
            ( m, Cmd.none )
    in
    case msg of
        NoOp ->
            model |> noCmd

        GotSeed seed ->
            model
                |> Shared.setSeed seed
                |> noCmd

        Fish seaFood ->
            model
                |> Shared.getFishingPool
                |> (\( head, tail ) -> Random.List.choose (seaFood :: head :: tail))
                |> Random.map
                    (\( maybe, list ) ->
                        ( maybe |> Maybe.withDefault SeaFood.default
                        , case list of
                            [] ->
                                ( SeaFood.default, [] )

                            head :: tail ->
                                ( head, tail )
                        )
                    )
                |> (\generator -> Random.step generator model.seed)
                |> (\( ( result, list ), seed ) ->
                        model
                            |> Shared.setSeed seed
                            |> Shared.setFishingPool list
                            |> update request (AddFood (Food.SeaFood result))
                   )

        AddToFishingPool seaFood ->
            ( model
                |> Shared.mapFishingPool (\( head, tail ) -> ( seaFood, head :: tail ))
            , Cmd.none
            )

        AddSeaFood ->
            model.fishingPool
                |> (\( head, tail ) -> Random.List.choose (head :: tail))
                |> Random.map
                    (\( maybe, list ) ->
                        ( maybe |> Maybe.withDefault SeaFood.default
                        , case list of
                            [] ->
                                ( SeaFood.default, [] )

                            head :: tail ->
                                ( head, tail )
                        )
                    )
                |> (\generator -> Random.step generator model.seed)
                |> (\( ( seaFood, list ), seed ) ->
                        model
                            |> Shared.setSeed seed
                            |> Shared.setFishingPool list
                            |> update request (AddFood (Food.SeaFood seaFood))
                   )

        AddFood item ->
            model |> Shared.mapFood (AnyBag.insert 1 item) |> noCmd

        RemoveFood amount item ->
            model |> Shared.mapFood (AnyBag.remove amount item) |> noCmd

        AddStone amount stone ->
            model
                |> Shared.mapStone (AnyBag.insert amount stone)
                |> noCmd

        AddField ->
            model |> Shared.mapFields (Array.push Nothing) |> noCmd

        Plant i vegi ->
            model |> Shared.mapFields (Array.set i (Just vegi)) |> noCmd

        RemovePlant i ->
            model |> Shared.mapFields (Array.set i Nothing) |> noCmd

        Rethrow ->
            Dice.random
                |> Random.list 3
                |> Random.map
                    (\list ->
                        model |> Shared.setDice (Dicebag.empty |> Dicebag.addAll list)
                    )
                |> applyGenerator model.seed
                |> noCmd

        AddMoney money ->
            model |> Shared.mapMoney ((+) money) |> noCmd

        AddRandomDice amount ->
            model.seed
                |> Random.step
                    (Dice.random
                        |> Random.list amount
                        |> Random.map (\list -> Dicebag.empty |> Dicebag.addAll list)
                    )
                |> (\( dice, seed ) -> { model | seed = seed } |> update request (AddDice dice))

        AddDice selectedDice ->
            model
                |> Shared.mapDice
                    (Dicebag.addAll
                        (selectedDice
                            |> Dicebag.toList
                            |> List.concatMap (\( i, n ) -> List.repeat n i)
                        )
                    )
                |> noCmd

        RemoveDice selectedDice ->
            model
                |> Shared.mapDice
                    (Dicebag.removeAll
                        (selectedDice
                            |> Dicebag.toList
                            |> List.concatMap (\( i, n ) -> List.repeat n i)
                        )
                    )
                |> noCmd


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none

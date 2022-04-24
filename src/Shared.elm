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
import Data.Food.Vegetable as Vegetable exposing (Vegetable)
import Json.Decode as Json
import Random exposing (Generator, Seed)
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { dice : DiceBag
    , money : Int
    , seed : Seed
    , items : AnyBag String Food
    , fields : Array (Maybe Vegetable)
    }


type Msg
    = NoOp
    | UpdateModel (Model -> Model)
    | AddItem Food
    | RemoveItem Food
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
      , items = AnyBag.empty Food.toString
      , fields = Array.fromList [ Nothing ]
      }
    , Random.independentSeed |> Random.generate (\seed -> UpdateModel (\m -> { m | seed = seed }))
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
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateModel fun ->
            ( fun model, Cmd.none )

        AddItem item ->
            ( { model | items = model.items |> AnyBag.insert 1 item }, Cmd.none )

        RemoveItem item ->
            ( { model | items = model.items |> AnyBag.remove 1 item }, Cmd.none )

        AddField ->
            ( { model | fields = model.fields |> Array.push Nothing }, Cmd.none )

        Plant i vegi ->
            ( { model | fields = model.fields |> Array.set i (Just vegi) }, Cmd.none )

        RemovePlant i ->
            ( { model | fields = model.fields |> Array.set i Nothing }, Cmd.none )

        Rethrow ->
            ( Dice.random
                |> Random.list 3
                |> Random.map
                    (\list ->
                        { model | dice = Dicebag.empty |> Dicebag.addAll list }
                    )
                |> applyGenerator model.seed
            , Cmd.none
            )

        AddMoney money ->
            ( { model | money = model.money + money }, Cmd.none )

        AddRandomDice amount ->
            model.seed
                |> Random.step
                    (Dice.random
                        |> Random.list amount
                        |> Random.map (\list -> Dicebag.empty |> Dicebag.addAll list)
                    )
                |> (\( dice, seed ) -> { model | seed = seed } |> update request (AddDice dice))

        AddDice selectedDice ->
            ( { model
                | dice =
                    model.dice
                        |> Dicebag.addAll
                            (selectedDice
                                |> Dicebag.toList
                                |> List.concatMap (\( i, n ) -> List.repeat n i)
                            )
              }
            , Cmd.none
            )

        RemoveDice selectedDice ->
            ( { model
                | dice =
                    model.dice
                        |> Dicebag.removeAll
                            (selectedDice
                                |> Dicebag.toList
                                |> List.concatMap (\( i, n ) -> List.repeat n i)
                            )
              }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none

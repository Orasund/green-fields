module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Data.AnyBag as AnyBag exposing (AnyBag)
import Data.Dice as Dice
import Data.DiceBag as Dicebag exposing (DiceBag)
import Data.Food as Food exposing (Food)
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
    }


type Msg
    = NoOp
    | UpdateModel (Model -> Model)
    | AddItem Food
    | AddMoney Int
    | RemoveDice DiceBag
    | Rethrow


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { dice = Dicebag.empty
      , money = 0
      , seed = Random.initialSeed 42
      , items = AnyBag.empty Food.toString
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
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateModel fun ->
            ( fun model, Cmd.none )

        AddItem item ->
            ( { model | items = model.items |> AnyBag.insert 1 item }, Cmd.none )

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

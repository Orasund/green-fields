module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Data.Dice as Dice
import Data.DiceBag as Dicebag exposing (DiceBag)
import Json.Decode as Json
import Random exposing (Generator, Seed)
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { dice : DiceBag
    , seed : Seed
    }


type Msg
    = NoOp
    | UpdateModel (Model -> Model)
    | Rethrow


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { dice = Dicebag.empty, seed = Random.initialSeed 42 }
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


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none

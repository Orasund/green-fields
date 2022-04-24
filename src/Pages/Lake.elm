module Pages.Lake exposing (Model, Msg, page)

import Config
import Data.DiceBag as DiceBag exposing (DiceBag)
import Data.Die as Dice exposing (Die)
import Data.Food as Food
import Data.Food.Fish as Fish
import Effect exposing (Effect)
import Gen.Params.Lake exposing (Params)
import Html.Styled as Html exposing (Html)
import Page
import Request
import Shared
import StaticArray.Index as Index
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
    = Catch (List Die)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Catch list ->
            ( model
            , [ list
                    |> List.map (\dice -> ( dice, 1 ))
                    |> DiceBag.fromList
                    |> Shared.RemoveDice
                    |> Effect.fromShared
                    |> Just
              , list
                    |> List.length
                    |> Fish.fromStreet
                    |> Maybe.map
                        (\fish ->
                            fish
                                |> Food.FishFood
                                |> Shared.AddItem
                                |> Effect.fromShared
                        )
              ]
                |> List.filterMap identity
                |> Effect.batch
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewStreet : List Die -> Maybe (Html Msg)
viewStreet list =
    list
        |> List.length
        |> Fish.fromStreet
        |> Maybe.map
            (\fish ->
                let
                    name =
                        fish |> Fish.toString
                in
                Style.section ("Catch " ++ name)
                    [ [ "Use "
                            ++ (list
                                    |> List.map Dice.toString
                                    |> String.concat
                               )
                            ++ " to "
                            |> Html.text
                      , Catch list |> Just |> Style.button ("Catch a " ++ name)
                      , "." |> Html.text
                      ]
                        |> Style.row
                    ]
            )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Lake"
    , body =
        if DiceBag.isEmpty shared.dice then
            [ NoDice.view ]

        else
            case
                shared.dice
                    |> DiceBag.streets
                    |> List.filterMap viewStreet
            of
                [] ->
                    [ "You could not catch a fish today. "
                        ++ "Come back once you have a street of 3 or more die to catch some fish."
                        |> Style.paragraph
                    ]

                list ->
                    list
    }

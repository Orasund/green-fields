module Pages.Lake exposing (Model, Msg, page)

import Config
import Data.Dice as Dice exposing (Dice)
import Data.DiceBag as DiceBag exposing (DiceBag)
import Data.Food as Food
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
    = Catch (List Dice)


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
                    |> Food.fromStreet
                    |> Maybe.map
                        (\food ->
                            food
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


viewStreet : List Dice -> Maybe (Html Msg)
viewStreet list =
    list
        |> List.length
        |> Food.fromStreet
        |> Maybe.map
            (\food ->
                let
                    name =
                        food |> Food.toString
                in
                Style.section ("Catch " ++ name)
                    [ "Use "
                        ++ (list
                                |> List.map Dice.toString
                                |> String.concat
                           )
                        ++ " to catch a "
                        ++ name
                        |> Html.text
                    , Catch list |> Just |> Style.button ("Catch " ++ name)
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
                    [ "You are lucky today. " |> Style.paragraph
                    ]
                        ++ list
    }

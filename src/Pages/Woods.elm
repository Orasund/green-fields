module Pages.Woods exposing (Model, Msg, page)

import Data.Dice exposing (Dice)
import Data.DiceBag as DiceBag
import Effect exposing (Effect)
import Gen.Params.Woods exposing (Params)
import Page
import Request
import Shared
import View exposing (View)


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
    ( { dice = DiceBag.empty }, Effect.none )



-- UPDATE


type Msg
    = SelectDie Dice
    | UnselectDie Dice
    | ChopWood


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SelectDie die->
            ( {model | dice = model.dice |> DiceBag.add die }, Effect.none )
        UnselectDie die ->
            ( {model | dice = model.dice |> DiceBag.remove die }, Effect.none )
        ChopWood ->
            Debug.todo "add Chopwood function"

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = Config.title
    , body =
        [ Style.section "Woods"
            [ "You can use leftover dice to choop some wood."
                |> Style.paragraph
            {--, Html.table [] 
                [ Html.tr [] [
                    Html.td []
                ]]--}
            , Style.button "Throw Dice" ChopWood
            ]
        
        ]
    }

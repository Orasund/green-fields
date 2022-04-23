module Pages.Kitchen exposing (Model, Msg, page)

import Config
import Css
import Data.Dice as Dice
import Data.DiceBag as DiceBag
import Effect exposing (Effect)
import Gen.Params.Kitchen exposing (Params)
import Gen.Route as Route
import Html.Styled as Html
import Html.Styled.Attributes as Attr
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
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = Rethrow


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Rethrow ->
            ( model, Shared.Rethrow |> Effect.fromShared )



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    let
        diceBag =
            shared.dice
                |> DiceBag.toList
    in
    { title = Config.title
    , body =
        [ Style.section "Kitchen"
            (if List.isEmpty diceBag then
                [ "You don't have any dice."
                    |> Style.paragraph
                , Style.button "Throw Dice" Rethrow
                ]

             else
                [ shared.dice
                    |> DiceBag.toString
                    |> Html.text
                    |> List.singleton
                    |> Html.h2 []
                    |> List.singleton
                    |> Html.div [ Attr.css [ Css.textAlign Css.center ] ]
                ]
            )
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

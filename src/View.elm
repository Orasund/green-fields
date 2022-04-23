module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Config
import Css
import Data.Dice as Dice
import Data.DiceBag as DiceBag
import Gen.Route as Route
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Shared exposing (Model)
import View.Style as Style


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Html.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body =
        List.map (Html.map fn) view.body
    }


toBrowserDocument : Model -> View msg -> Browser.Document msg
toBrowserDocument model view =
    { title = Config.title
    , body =
        [ Html.node "link" [ Attr.rel "stylesheet", Attr.href "https://vanillacss.com/vanilla.css" ] []
        , Style.filledRow
            [ Style.link "Kitchen" Route.Kitchen
            , Style.link "Lake" Route.Lake
            , Style.link "Woods" Route.Woods
            ]
        , Style.filledRow
            [ model.dice
                |> DiceBag.toString
                |> Html.text
                |> List.singleton
                |> Html.div []
            , (model.money |> String.fromInt) ++ "G" |> Html.text |> List.singleton |> Html.div []
            ]
        , Style.article view.title view.body
        ]
            |> List.map Html.toUnstyled
    }

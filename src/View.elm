module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Config
import Css
import Data.DiceBag as DiceBag
import Data.Die as Dice
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
        [ Html.node "link" [ Attr.rel "stylesheet", Attr.href "vanilla.css" ] []
        , Style.filledRow
            [ model.dice
                |> DiceBag.toString
                |> Html.text
                |> Style.elem
            , (model.money |> String.fromInt) ++ Config.moneySymbol |> Html.text |> Style.elem
            ]
        , Style.bold "Locations: "
            :: ([ Style.link Route.Kitchen
                , Style.link Route.Lake
                , Style.link Route.Mine
                , Style.link Route.Fields
                , Style.link Route.Woods
                ]
                    |> List.intersperse (Html.text " ")
               )
            |> Html.div []
        , Style.article view.title view.body
        , Style.paragraph ""
        ]
            |> List.map Html.toUnstyled
    }

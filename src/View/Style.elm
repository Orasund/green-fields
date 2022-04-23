module View.Style exposing (..)

import Css
import Gen.Route exposing (Route)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Shared exposing (Msg)


filledRow : List (Html msg) -> Html msg
filledRow =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.num 1 |> Css.flex
            ]
        , Attr.id "top"
        ]


paragraph : String -> Html msg
paragraph string =
    Html.p [] [ Html.text string ]


section : String -> List (Html msg) -> Html msg
section headerString content =
    Html.article []
        [ Html.header [] [ Html.h1 [] [ Html.text headerString ] ]
        , Html.div [] content
        ]


button : String -> msg -> Html msg
button string msg =
    Html.a [ Attr.href "#", Events.onClick msg ] [ "[" ++ string ++ "]" |> Html.text ]


link : String -> Route -> Html msg
link string route =
    Html.a [ Attr.href (route |> Gen.Route.toHref) ] [ Html.text string ]

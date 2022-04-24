module View.Style exposing (..)

import Css
import Data.Route as Route
import Gen.Route exposing (Route)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Shared exposing (Msg)


none : Html msg
none =
    Html.text ""


bold : String -> Html msg
bold string =
    string
        |> Html.text
        |> List.singleton
        |> Html.span [ Attr.css [ Css.fontWeight Css.bold ] ]


list : List (Html msg) -> Html msg
list content =
    content
        |> List.map (\e -> [ e ] |> Html.li [])
        |> Html.ul []


table : List String -> List (List (Html msg)) -> Html msg
table header rows =
    (header
        |> List.map
            (\e ->
                e
                    |> Html.text
                    |> List.singleton
                    |> Html.th []
            )
        |> Html.tr []
    )
        :: (rows
                |> List.map
                    (\r ->
                        r
                            |> List.map
                                (\e ->
                                    e
                                        |> List.singleton
                                        |> Html.td []
                                )
                            |> Html.tr []
                    )
           )
        |> Html.table []


filledRow : List (Html msg) -> Html msg
filledRow =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.num 1 |> Css.flex
            ]
        ]


elem : Html msg -> Html msg
elem content =
    content
        |> List.singleton
        |> Html.div []


row : List (Html msg) -> Html msg
row =
    Html.p []


column : List (Html msg) -> Html msg
column content =
    content
        |> List.map elem
        |> Html.div []


paragraph : String -> Html msg
paragraph string =
    Html.p [] [ Html.text string ]


section : String -> List (Html msg) -> Html msg
section headerString content =
    Html.h2 [] [ Html.text headerString ]
        :: content
        |> Html.section []


article : String -> List (Html msg) -> Html msg
article title content =
    Html.article []
        [ Html.header [] [ Html.h1 [] [ Html.text title ] ]
        , Html.div [] content
        ]


button : String -> Maybe msg -> Html msg
button string maybeMsg =
    case maybeMsg of
        Just msg ->
            Html.a [ Attr.href "#", Events.onClick msg ] [ "[" ++ string ++ "]" |> Html.text ]

        Nothing ->
            "[" ++ string ++ "]" |> Html.text


link : Route -> Html msg
link route =
    Html.a [ Attr.href (route |> Gen.Route.toHref) ] [ Html.text (Route.toString route) ]

module View.Style exposing (..)

import Css
import Gen.Route exposing (Route)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Shared exposing (Msg)


table : List String -> List (List (Html msg)) -> Html msg
table header rows =
    (header
        |> List.map
            (\elem ->
                elem
                    |> Html.text
                    |> List.singleton
                    |> Html.th []
            )
        |> Html.tr []
    )
        :: (rows
                |> List.map
                    (\row ->
                        row
                            |> List.map
                                (\elem ->
                                    elem
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


link : String -> Route -> Html msg
link string route =
    Html.a [ Attr.href (route |> Gen.Route.toHref) ] [ Html.text string ]

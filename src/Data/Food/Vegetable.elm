module Data.Food.Vegetable exposing (..)

import Data.Die as Die exposing (Die)


type Vegetable
    = Potato
    | Carrot
    | Tomato
    | Onion
    | Cucumber
    | Broccoli


toString : Vegetable -> String
toString vegetable =
    emoji vegetable
        ++ (case vegetable of
                Potato ->
                    "Potato"

                Carrot ->
                    "Carrot"

                Tomato ->
                    "Tomato"

                Onion ->
                    "Onion"

                Cucumber ->
                    "Cucumber"

                Broccoli ->
                    "Cauliflower"
           )


emoji : Vegetable -> String
emoji vegetable =
    case vegetable of
        Potato ->
            "🥔"

        Carrot ->
            "🥕"

        Tomato ->
            "🍅"

        Onion ->
            "🧅"

        Cucumber ->
            "🥒"

        Broccoli ->
            "🥦"


asList : List Vegetable
asList =
    [ Potato, Carrot, Tomato, Onion, Cucumber, Broccoli ]


toDie : Vegetable -> Die
toDie vegetable =
    (case vegetable of
        Potato ->
            0

        Carrot ->
            1

        Tomato ->
            2

        Onion ->
            3

        Cucumber ->
            4

        Broccoli ->
            5
    )
        |> Die.fromInt


modifier : Vegetable -> Int
modifier vegetable =
    case vegetable of
        Potato ->
            -3

        Carrot ->
            -2

        Tomato ->
            -1

        Onion ->
            1

        Cucumber ->
            2

        Broccoli ->
            3

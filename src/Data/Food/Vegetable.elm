module Data.Food.Vegetable exposing (..)

import Data.Die as Die exposing (Die)


type Vegetable
    = Potato
    | Carrot
    | Salat
    | Union
    | Cucumber
    | Cauliflower


toString : Vegetable -> String
toString vegetable =
    case vegetable of
        Potato ->
            "Potato"

        Carrot ->
            "Carrot"

        Salat ->
            "Salat"

        Union ->
            "Union"

        Cucumber ->
            "Cucumber"

        Cauliflower ->
            "Cauliflower"


asList : List Vegetable
asList =
    [ Potato, Carrot, Salat, Union, Cucumber, Cauliflower ]


toDie : Vegetable -> Die
toDie vegetable =
    (case vegetable of
        Potato ->
            0

        Carrot ->
            1

        Salat ->
            2

        Union ->
            3

        Cucumber ->
            4

        Cauliflower ->
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

        Salat ->
            -1

        Union ->
            1

        Cucumber ->
            2

        Cauliflower ->
            3

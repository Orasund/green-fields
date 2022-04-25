module Data.Food.SeaFood exposing (..)

import Config
import Gen.Enum.SeaFood


type Fish
    = Fish
    | Octopus
    | Lobster


asList : List Fish
asList =
    [ Fish, Octopus, Lobster ]


emoji : Fish -> String
emoji fish =
    case fish of
        Fish ->
            "🐟"

        Octopus ->
            "🐙"

        Lobster ->
            "🦞"


toString : Fish -> String
toString fish =
    emoji fish
        ++ (case fish of
                Fish ->
                    "Fish"

                Octopus ->
                    "Octopus"

                Lobster ->
                    "Lobster"
           )


price : Fish -> Int
price fish =
    2 ^ (toAmount fish - 1) * Config.fishBasePrice


fromStreet : Int -> Maybe Fish
fromStreet streetLength =
    if streetLength == 3 then
        Just Fish

    else if streetLength == 4 then
        Just Octopus

    else if streetLength == 5 then
        Just Lobster

    else
        Nothing


toAmount : Fish -> Int
toAmount fish =
    case fish of
        Fish ->
            1

        Octopus ->
            2

        Lobster ->
            3

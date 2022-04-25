module Data.Food.SeaFood exposing (..)

import Config
import Gen.Enum.SeaFood exposing (SeaFood(..))


type alias SeaFood =
    Gen.Enum.SeaFood.SeaFood


asList : List SeaFood
asList =
    Gen.Enum.SeaFood.asList


emoji : SeaFood -> String
emoji fish =
    case fish of
        Fish ->
            "🐟"

        Octopus ->
            "🐙"

        Lobster ->
            "🦞"


toString : SeaFood -> String
toString fish =
    emoji fish ++ Gen.Enum.SeaFood.toString fish


price : SeaFood -> Int
price fish =
    2 ^ (toAmount fish - 1) * Config.fishBasePrice


fromStreet : Int -> Maybe SeaFood
fromStreet streetLength =
    if streetLength == 3 then
        Just Fish

    else if streetLength == 4 then
        Just Octopus

    else if streetLength == 5 then
        Just Lobster

    else
        Nothing


toAmount : SeaFood -> Int
toAmount fish =
    case fish of
        Fish ->
            1

        Octopus ->
            2

        Lobster ->
            3

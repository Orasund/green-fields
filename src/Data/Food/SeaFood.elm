module Data.Food.SeaFood exposing (..)

import Config
import Gen.Enum.SeaFood exposing (SeaFood(..))


type alias SeaFood =
    Gen.Enum.SeaFood.SeaFood


asList : List SeaFood
asList =
    Gen.Enum.SeaFood.asList


first : SeaFood
first =
    Gen.Enum.SeaFood.first


toEmoji : SeaFood -> String
toEmoji fish =
    case fish of
        Crab ->
            "🦀"

        Fish ->
            "🐟"

        Octopus ->
            "🐙"

        Lobster ->
            "🦞"


toString : SeaFood -> String
toString fish =
    toEmoji fish ++ Gen.Enum.SeaFood.toString fish


price : SeaFood -> Int
price fish =
    2 ^ toAmount fish * Config.fishBasePrice


fromInt : Int -> Maybe SeaFood
fromInt =
    Gen.Enum.SeaFood.fromInt


fromStreet : Int -> SeaFood
fromStreet streetLength =
    if streetLength < 3 then
        Gen.Enum.SeaFood.first

    else
        streetLength
            - 3
            |> Gen.Enum.SeaFood.fromInt
            |> Maybe.withDefault Gen.Enum.SeaFood.last


toAmount : SeaFood -> Int
toAmount =
    Gen.Enum.SeaFood.toInt

module Data.Food.Vegetable exposing (..)

import Data.Die as Die exposing (Die)
import Gen.Enum.Vegetable as Vegetable exposing (Vegetable(..))


type alias Vegetable =
    Vegetable.Vegetable


toString : Vegetable -> String
toString vegetable =
    emoji vegetable ++ Vegetable.toString vegetable


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
    Vegetable.asList


toDie : Vegetable -> Die
toDie vegetable =
    vegetable
        |> Vegetable.toInt
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

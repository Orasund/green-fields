module Data.Food exposing (..)

import Config
import Data.Die as Die exposing (Die)
import Data.Food.SeaFood as Fish exposing (SeaFood)
import Data.Food.Vegetable as Vegetable exposing (Vegetable)
import Json.Decode exposing (int)


type Food
    = SeaFood SeaFood
    | VegetableFood Vegetable


toString : Food -> String
toString food =
    case food of
        SeaFood fish ->
            fish |> Fish.toString

        VegetableFood vegetable ->
            vegetable |> Vegetable.toString


emoji : Food -> String
emoji food =
    case food of
        SeaFood fish ->
            fish |> Fish.toEmoji

        VegetableFood vegetable ->
            vegetable |> Vegetable.emoji


price : Food -> Int
price food =
    case food of
        SeaFood fish ->
            Fish.price fish

        VegetableFood vegetable ->
            Config.priceOfVegetables


description : Food -> String
description food =
    case food of
        SeaFood fish ->
            case Fish.toAmount fish of
                0 ->
                    "Can be sold."

                int ->
                    "Add " ++ String.fromInt int ++ " temporary dice."

        VegetableFood vegetable ->
            (vegetable
                |> Vegetable.modifier
                |> (\int ->
                        if int > 0 then
                            "+" ++ String.fromInt int

                        else
                            String.fromInt int
                   )
            )
                ++ " on any die."

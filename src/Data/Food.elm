module Data.Food exposing (..)

import Config
import Data.Die as Die exposing (Die)
import Data.Food.SeaFood as Fish exposing (Fish)
import Data.Food.Vegetable as Vegetable exposing (Vegetable)


type Food
    = FishFood Fish
    | VegetableFood Vegetable


toString : Food -> String
toString food =
    case food of
        FishFood fish ->
            fish |> Fish.toString

        VegetableFood vegetable ->
            vegetable |> Vegetable.toString


emoji : Food -> String
emoji food =
    case food of
        FishFood fish ->
            fish |> Fish.emoji

        VegetableFood vegetable ->
            vegetable |> Vegetable.emoji


price : Food -> Int
price food =
    case food of
        FishFood fish ->
            Fish.price fish

        VegetableFood vegetable ->
            Config.priceOfVegetables


description : Food -> String
description food =
    case food of
        FishFood fish ->
            "Add " ++ String.fromInt (Fish.toAmount fish) ++ " temporary dice."

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

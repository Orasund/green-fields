module Data.Food exposing (..)

import Data.Die as Die exposing (Die)
import Data.Food.Fish as Fish exposing (Fish)
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


price : Food -> Int
price food =
    case food of
        FishFood fish ->
            Fish.price fish

        VegetableFood vegetable ->
            5


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

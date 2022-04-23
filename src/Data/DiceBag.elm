module Data.DiceBag exposing (..)

import Data.Dice as Dice exposing (Dice)
import Random exposing (Generator)
import StaticArray exposing (StaticArray)
import StaticArray.Index exposing (Index, Six)
import StaticArray.Length as Length


type alias DiceBag =
    StaticArray Six Int


empty : DiceBag
empty =
    StaticArray.fromList (Length.plus1 Length.five) 0 []


addAll : List Dice -> DiceBag -> DiceBag
addAll list diceBag =
    list
        |> List.foldl (add) diceBag

add : Dice ->DiceBag -> DiceBag
add i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) 1)

remove : Dice -> DiceBag -> DiceBag
remove i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) -1 |> max 0)

toList : DiceBag -> List ( Dice, Int )
toList diceBag =
    diceBag
        |> StaticArray.indexedMap (\i n -> ( i, n ))
        |> StaticArray.toList
        |> List.filter (\( _, n ) -> n > 0)


toString : DiceBag -> String
toString diceBag =
    diceBag
        |> toList
        |> List.map
            (\( i, n ) ->
                List.repeat n i
                    |> List.map Dice.toString
                    |> String.concat
            )
        |> String.join " "
